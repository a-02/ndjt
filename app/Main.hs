{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Command
import Logging
import Parse
import Types
import Util

import Control.Monad
import Control.Monad.Trans.RWS.Strict

import Colog.Core.Action
import Colog.Core.IO

import Data.ByteString.Char8 qualified as BSC8
import Data.Digest.Adler32
import Data.Foldable
import Data.Function hiding (on)
import Data.List.NonEmpty qualified as NE
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

rnsServerPort :: Int
rnsServerPort = 6066

main :: IO ()
main = do
  -- grabbing eveyrthing

  (args :: [BSC8.ByteString])  <- getArgs
  rightNow <- getCurrentTime
  let now = formatTime defaultTimeLocale "%s" rightNow
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
  deckSockets <- zipWithM openUdp (T.unpack <$> decks) ports
  logByteStringLn logNetworkHandle <& "created deck sockets"

  -- setting up initial state and read-only constants

  drawLanding vty decks
  let info = NDJTInfo{vty, logMainHandle, logNetworkHandle, deckSockets}
      activeDecks = listToZipper $ 
        DeckInfo <$> deckSockets <*> fix (False:) <*> homeDirectories
      st = NDJTState{deckSwitches = activeDecks, mode = FileLoader ""}

  -- run main loop, exit when done

  _ <- execRWST vtyGO info st
  shutdown vty

vtyGO :: App ()
vtyGO = do
  info <- ask
  --  mode <- get
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
  key & case st.mode of
    FileLoader fl -> interpretFileLoader fl
    InputAsHash ih -> interpretInputAsHash ih
    TreatAsBitstring bs -> interpretTreatAsBitstring bs
    QueueBuffer qb -> interpretQueueBuffer qb

changeMode :: Int -> App ()
changeMode i = do 
    vty <- (.vty) <$> ask
    st <- get
    let decks = deckSwitches st
    case i of
      1 -> do
        put $ st{mode = FileLoader ""}
        drawFileLoader
      2 -> do
        put $ st{mode = InputAsHash ""}
        drawInputHash "" ""
      3 -> do
        put $ st{mode = TreatAsBitstring zeroWord256}
        drawBitstring zeroWord256
      4 -> do
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
        handle <- (.logMainHandle) <$> ask -- Woah!
        logStringHandle handle <& changeModeErrorMessage
        liftIO exitFailure

interpretFileLoader :: BSC8.ByteString -> Key -> App ()
interpretFileLoader file key = do
  handle <- (.logMainHandle) <$> ask
  st <- get
  if key == KEnter
    then do 
      logStringHandle handle <& "got to interpretFileLoader KEnter"
      mapM_ (\(DeckInfo udp active directory) -> when active (load file udp directory)) st.deckSwitches
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

drawFileLoader :: App ()
drawFileLoader = do
  vty <- (.vty) <$> ask
  st <- get
  let line1 = Vty.string (defAttr `withForeColor` green) (fromFileLoader st.mode)
      deckActivesShow = show st.deckSwitches
      line2 = Vty.string (defAttr `withForeColor` red) deckActivesShow
      line3 = Vty.string (defAttr `withForeColor` yellow) "You are in File Loader mode."
  liftIO $ update vty (picForImage $ foldl1 vertJoin ([line1, line2, line3] :: [Image]))

fromFileLoader :: OperatingMode -> String
fromFileLoader = \case
  FileLoader fl -> BSC8.unpack fl
  _ -> ""

drawLanding :: Vty -> [T.Text] -> IO ()
drawLanding vty decks = do
  update vty (picForImage $ rainbowImage landingText)
 where
  landingText = block1 `T.append` deckText `T.append` block2
  block1 = T.unlines
    [ "Welcome to the NKS Renoise Multitool!"
    , "(c) Infoglames, 2023-2024"
    , "5000 S. Halsted St. Chicago, IL 60609"
    , ""
    , "You are connected to:"
    ]
  deckText = T.unlines decks 
  block2 = T.unlines
    [ ""
    , "F1: File Loader mode. Use this to load XRNS files into NDJT."
    , "F2: Adler-32 mode. Control track solos with Adler-32 hash."
    , "F3: Bitstring mode. Control track solos as if it were a bitstring."
    , "F4: Queue Buffer mode. Dedicated sequence scheduler."
    , "ESC: Close NDJT."
    ]

rainbowImage :: T.Text -> Image
rainbowImage tx =
  foldl1 vertJoin $
    (\(a, b) -> Vty.text' (defAttr `withForeColor` b) a) <$> zip (T.lines tx) themeColors

themeColors :: [Color]
themeColors =
  let u3 f (a, b, c) = f a b c
   in u3 linearColor <$> Prelude.reverse [(i, j, k) | (i :: Int) <- [0, 64 .. 255], j <- [255, 224 .. 0], k <- [0, 64 .. 255]]

-- holy shit whens the last time i used a list comprehension?

interpretTreatAsBitstring :: Word256 -> Key -> App ()
interpretTreatAsBitstring w256 key = do
  decks <- (.deckSockets) <$> ask
  st <- get
  drawBitstring w256
  mapM_ (playTracks w256) decks
  case key of
    KChar 'q' -> put $ st { mode = TreatAsBitstring $ complement w256 }
    KChar 'a' -> put $ st { mode = TreatAsBitstring $ rotateL w256 1 }
    KChar 'd' -> put $ st { mode = TreatAsBitstring $ rotateR w256 1 }
    KChar 'A' -> put $ st { mode = TreatAsBitstring $ rotateL w256 10 }
    KChar 'D' -> put $ st { mode = TreatAsBitstring $ rotateR w256 10 }
    _ -> return ()

drawBitstring :: Word256 -> App ()
drawBitstring w256 = do
  let line1 = Vty.string (defAttr `withForeColor` blue) (show w256)
      line2 = Vty.string (defAttr `withForeColor` yellow) "You are in Bitstring mode."
  vty <- (.vty) <$> ask
  liftIO $ update vty (picForImage $ foldl1 vertJoin ([line1, line2] :: [Image]))

interpretQueueBuffer :: Zipper Int -> Key -> App ()
interpretQueueBuffer qb key = do
  vty <- (.vty) <$> ask
  st <- get
  case key of
    KEnter -> mapM_ (\(DeckInfo udp active _) -> when active (addScheduledSequence (toList qb) udp)) st.deckSwitches
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

drawQueueBuffer :: Zipper Int -> Decks -> App ()
drawQueueBuffer qb decks = do
  vty <- (.vty) <$> ask
  let line1 = Vty.string (defAttr `withForeColor` cyan) (show qb)
      line2 = Vty.string (defAttr `withForeColor` magenta) (show $ (\DeckInfo{..} -> udpSocket conn) <$> decks)
      line3 = Vty.string (defAttr `withForeColor` yellow) "You are in Queue Buffer mode."
  liftIO $ update vty (picForImage $ foldl1 vertJoin ([line1, line2, line3] :: [Image]))

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
      decks <- (.deckSockets) <$> ask
      mapM_ (playTracks . adler $ BSC8.singleton ch) decks
    _ -> return ()

drawInputHash :: BSC8.ByteString -> BSC8.ByteString -> App ()
drawInputHash msg adler = do
  vty <- (.vty) <$> ask
  let msgLine = Vty.string (defAttr `withForeColor` cyan) (BSC8.unpack msg)
      adlerLine = Vty.string (defAttr `withForeColor` magenta) (BSC8.unpack adler)
      statsLine = Vty.string (defAttr `withForeColor` yellow) "You are in Adler-32 mode."
  liftIO $ update vty (picForImage $ foldl1 vertJoin ([msgLine, adlerLine, statsLine] :: [Image]))

changeModeErrorMessage :: String
changeModeErrorMessage =
  unlines
    [ "Error: You have an F13 key! Why!?"
    ]
