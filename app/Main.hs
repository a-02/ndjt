{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Command
import Draw
import Logging
import Parse
import Types
import Util

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.RWS.Strict

import Colog.Core.Action
import Colog.Core.IO

import Data.ByteString.Char8 qualified as BSC8
import Data.Digest.Adler32
import Data.Foldable
import Data.Function hiding (on)
import Data.List.NonEmpty.Zipper as Z
import Data.Time
import Data.Text qualified as T
import Data.WideWord.Word256

import GHC.Bits

import Graphics.Vty as Vty
import Graphics.Vty.Platform.Unix (mkVty)

import Net.IP as IP

import Sound.Osc

import System.Directory
import System.Exit
import System.IO
import System.Posix.Env.ByteString

main :: IO ()
main = do
  -- grabbing eveyrthing

  (args :: [BSC8.ByteString])  <- getArgs
  rightNow <- getCurrentTime
  let now = formatTime defaultTimeLocale "%T" rightNow
      filename = "log/ndjt_" ++ now
      logMain = filename ++ "/main"
      logNetwork = filename ++ "/network"

  -- setting up logging dirs

  createDirectoryIfMissing True filename
  logMainHandle <- openFile logMain WriteMode
  logNetworkHandle <- openFile logNetwork WriteMode
  let handles :: [Handle] = [logMainHandle, logNetworkHandle, stdout, stdin]
  mapM_ (`hSetBuffering` NoBuffering) handles

  -- make vty

  vty <- userConfig >>= mkVty

  -- make udp connections

  argsRes <- processArgs logNetworkHandle args

  let ports = fromIntegral . argPort <$> argsRes
      decks = IP.encode . argIP <$> argsRes
      homeDirectories = argHome <$> argsRes
      argsTextRep = argText <$> argsRes
  deckSockets <- zipWithM openUdp (T.unpack <$> decks) ports
  logByteStringLn logNetworkHandle <& "created deck sockets"

  -- setting up initial state and read-only constants

  drawLanding vty argsTextRep
  let info = NDJTInfo{vty, logMainHandle, logNetworkHandle}
      activeDecks = listToZipper . getZipList $ 
        DeckInfo 
          <$> ZipList deckSockets 
          <*> ZipList (replicate (length homeDirectories) False) 
          <*> ZipList homeDirectories 
          <*> ZipList argsRes
          -- pissing me off.
      st = NDJTState{deckSwitches = activeDecks, mode = FileLoader ""}

  -- run main loop, exit when done

  logByteStringLn logMainHandle <& BSC8.unlines homeDirectories
  logByteStringLn logMainHandle <& BSC8.pack (unlines $ show <$> argsRes)
  logTextLn logMainHandle <& showDecks activeDecks
  _ <- execRWST vtyGO info st
  shutdown vty

vtyGO :: App ()
vtyGO = do
  info <- ask
  k <- liftIO $ collapseEventToKey <$> nextEvent info.vty
  either (\x -> logStringHandle info.logMainHandle <& x) interpretKey k
  unless (k == Left "escape") vtyGO

collapseEventToKey :: Event -> Either String Key
collapseEventToKey (EvKey KEsc []) = Left "escape"
collapseEventToKey (EvKey k _) = Right k
collapseEventToKey a = Left $ "got " ++ show a

interpretKey :: Key -> App ()
interpretKey key = do
  case key of
    (KFun i) -> changeMode i
    _ -> modeBranch key

modeBranch :: Key -> App ()
modeBranch key = do
  st <- get
  handle <- (.logMainHandle) <$> ask -- Woah!
  logByteStringLn handle <& "modeBranch called"
  key & case st.mode of
    FileLoader fl -> interpretFileLoader fl
    InputAsHash ih -> interpretInputAsHash ih
    TreatAsBitstring bs -> interpretTreatAsBitstring bs
    QueueBuffer qb -> interpretQueueBuffer qb

changeMode :: Int -> App ()
changeMode i = do 
    vty <- (.vty) <$> ask
    handle <- (.logMainHandle) <$> ask -- Woah!
    st <- get
    let decks = deckSwitches st
    case i of
      1 -> do
        logByteStringLn handle <& "switching to FileLoader"
        put $ st{mode = FileLoader ""}
        drawFileLoader
      2 -> do
        logByteStringLn handle <& "switching to InputAsHash"
        put $ st{mode = InputAsHash ""}
        drawInputHash "" ""
      3 -> do
        logByteStringLn handle <& "switching to TreatAsBitstring"
        put $ st{mode = TreatAsBitstring zeroWord256}
        drawBitstring zeroWord256
      4 -> do
        logByteStringLn handle <& "switching to QueueBuffer"
        put $ st{mode = QueueBuffer $ listToZipper [0]}
        drawQueueBuffer (listToZipper [0]) decks
      5 -> do
        drawFileLoader
      6 -> do
        drawInputHash "" ""
      7 -> do
        drawBitstring zeroWord256
      8 -> do
        drawQueueBuffer (listToZipper [0]) decks
      9 -> do
        drawFileLoader
      10 -> do
        drawInputHash "" ""
      11 -> do
        drawBitstring zeroWord256
      12 -> do
        drawQueueBuffer (listToZipper [0]) decks
      _ -> do
        logStringHandle handle <& changeModeErrorMessage
        liftIO exitFailure

interpretFileLoader :: BSC8.ByteString -> Key -> App ()
interpretFileLoader file key = do
  handle <- (.logMainHandle) <$> ask
  st <- get
  if key == KEnter
    then do 
      logByteStringLn handle <& "got to interpretFileLoader KEnter"
      mapM_ (\(DeckInfo udp active directory _) -> when active (load file udp directory)) st.deckSwitches
    else do
      case key of
        KChar ch -> do
          put $ st { mode = FileLoader $ file `BSC8.snoc` ch}
        KBS -> maybe (return ()) ((\x -> put $ st { mode = FileLoader x }) . fst) (BSC8.unsnoc file) -- ugh
        KRight -> maybe (return ()) (\x -> put $ st { deckSwitches = x }) (right st.deckSwitches)
        KLeft -> maybe (return ()) (\x -> put $ st { deckSwitches = x }) (left st.deckSwitches)
        KUp -> put $ st { deckSwitches = on st.deckSwitches }
        KDown -> put $ st { deckSwitches = off st.deckSwitches }
        _ -> return ()
  drawFileLoader


-- holy shit whens the last time i used a list comprehension?

interpretTreatAsBitstring :: Word256 -> Key -> App ()
interpretTreatAsBitstring w256 key = do
  st <- get
  drawBitstring w256
  mapM_ (playTracks w256) (conn <$> st.deckSwitches)
  case key of
    KChar 'q' -> put $ st { mode = TreatAsBitstring $ complement w256 }
    KChar 'a' -> put $ st { mode = TreatAsBitstring $ rotateL w256 1 }
    KChar 'd' -> put $ st { mode = TreatAsBitstring $ rotateR w256 1 }
    KChar 'A' -> put $ st { mode = TreatAsBitstring $ rotateL w256 10 }
    KChar 'D' -> put $ st { mode = TreatAsBitstring $ rotateR w256 10 }
    _ -> return ()

interpretQueueBuffer :: Zipper Int -> Key -> App ()
interpretQueueBuffer qb key = do
  vty <- (.vty) <$> ask
  st <- get
  case key of
    KEnter -> mapM_ (\(DeckInfo udp active _ _) -> when active (addScheduledSequence (toList qb) udp)) st.deckSwitches
    KRight -> maybe (return ()) (\x -> put $ st { deckSwitches = x }) (right st.deckSwitches)
    KLeft -> maybe (return ()) (\x -> put $ st { deckSwitches = x }) (left st.deckSwitches)
    KUp -> put $ st { deckSwitches = on st.deckSwitches }
    KDown -> put $ st { deckSwitches = off st.deckSwitches }
    KChar 'w' -> put $ st { mode = QueueBuffer $ add 1 qb }
    KChar 's' -> put $ st { mode = QueueBuffer $ add -1 qb }
    KChar 'a' -> maybe (return ()) (\x -> put $ st { mode = QueueBuffer x }) (left qb)
    KChar 'd' -> maybe (return ()) (\x -> put $ st { mode = QueueBuffer x }) (right qb)
    KChar 'W' -> put $ st { mode = QueueBuffer $ add 10 qb }
    KChar 'S' -> put $ st { mode = QueueBuffer $ add -10 qb }
    _ -> return ()

interpretInputAsHash :: BSC8.ByteString -> Key -> App ()
interpretInputAsHash ih key = do
  let (+++) = BSC8.append
      msg c = "MESSAGE --> " +++ ih +++ c
      adler c = adler32 $ ih +++ c
      adlerDigest = "ADLER-32 HASH --> " +++ (BSC8.pack . show $ adler "")
  drawInputHash (msg "") adlerDigest
  st <- get
  case key of
    (KChar ch) -> do
      put $ st { mode = InputAsHash (msg $ BSC8.singleton ch) }
      st' <- get -- this is lazy. you need to wake up, man.
      mapM_ (playTracks . adler $ BSC8.singleton ch) (conn <$> st'.deckSwitches)
    _ -> return ()

changeModeErrorMessage :: String
changeModeErrorMessage =
  unlines
    [ "Error: You have an F13 key! Why!?"
    ]
