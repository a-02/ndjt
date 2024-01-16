{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Command
import Types

import Control.Monad
import Control.Monad.Trans.RWS.Strict

import Colog.Core.Action
import Colog.Core.IO

import GHC.Bits
import Data.Bifunctor
import qualified Data.ByteString.Char8 as BSC8
import Data.Function hiding (on)
import Data.Digest.Adler32
import Data.List (init)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty.Zipper as Z
import Data.Time
import Data.WideWord.Word256

import Graphics.Vty as Vty

import Sound.Osc

import System.Directory
import System.IO
import System.Exit

rnsServerPort :: Int
rnsServerPort = 6066

-- make this ask for more.
init :: IO (NE.NonEmpty String)
init = do
  putStr "A DECK IP: "
  deckA <- getLine
{-
  putStr "B DECK IP: "
  deckB <- getLine
-}
  return $ NE.fromList [deckA]

main :: IO ()
main = do
  rightNow <- getCurrentTime
  let now = formatTime defaultTimeLocale "%s" rightNow
      filename = "log/ndjt_" ++ now
      logMain = filename ++ "/main"
      logNetwork = filename ++ "/network"
  createDirectoryIfMissing True filename
  logMainHandle <- openFile logMain WriteMode
  logNetworkHandle <- openFile logNetwork WriteMode
  mapM_ (`hSetBuffering` NoBuffering) [logMainHandle, logNetworkHandle, stdout, stdin]
  decks <- Main.init
  cfg <- standardIOConfig
  vty <- mkVty cfg
  logStringHandle logMainHandle <& ("got" ++ show decks)
  deckSockets <- mapM (`openTcp` rnsServerPort) decks
  logStringHandle logNetworkHandle <& "successfully got deckSockets?"
  let info = NDJTInfo {vty, logMainHandle, logNetworkHandle, deckSockets}
  _ <- execRWST vtyGO info (FileLoader "")
  shutdown vty

vtyGO :: App ()
vtyGO = do
  info <- ask
  mode <- get
  k <- liftIO $ collapseEventToKey <$> nextEvent info.vty
  either (\x -> logStringHandle info.logMainHandle <& x) interpretKey k
  unless (k == Left "escape") vtyGO

collapseEventToKey :: Event -> Either String Key
collapseEventToKey (EvKey KEsc []) = Left "escape"
collapseEventToKey (EvKey k _) = Right k
collapseEventToKey _ = Left "bro what"

interpretKey :: Key -> App ()
interpretKey key = do
  mode <- get
  case key of
    (KFun i) -> changeMode i
    _ -> modeBranch key mode

modeBranch :: Key -> OperatingMode -> App ()
modeBranch key mode = do
  decks <- (.deckSockets) <$> ask
  let fileLoaderActiveDecks = fromNonEmpty . NE.zip decks $ fix (False NE.<|)
  key & case mode of
    FileLoader fl -> interpretFileLoader fl fileLoaderActiveDecks
    InputAsHash ih -> interpretInputAsHash ih
    TreatAsBitstring bs -> interpretTreatAsBitstring bs
    QueueBuffer qb -> interpretQueueBuffer qb

changeMode :: Int -> App ()
changeMode = \case
  1 -> put $ FileLoader []
  2 -> put $ InputAsHash []
  3 -> put $ TreatAsBitstring zeroWord256
  4 -> put $ QueueBuffer []
  5 -> put $ QueueBuffer []
  6 -> put $ QueueBuffer []
  7 -> put $ QueueBuffer []
  8 -> put $ QueueBuffer []
  9 -> put $ QueueBuffer []
  10 -> put $ QueueBuffer []
  11 -> put $ QueueBuffer []
  12 -> put $ QueueBuffer []
  _   -> do handle <- (.logMainHandle) <$> ask -- Woah!
            logStringHandle handle <& changeModeErrorMessage
            liftIO exitFailure

interpretFileLoader :: String -> Zipper (Tcp, Bool) -> Key -> App ()
interpretFileLoader file deckActives key = do
  vty <- (.vty) <$> ask
  drawFileLoader file deckActives
  let go x = do 
        k <- liftIO $ collapseEventToKey <$> nextEvent vty
        interpretFileLoader file x (either error id k) -- oops! got lazy
      on z = replace (second (const True) $ current z) z
      off z = replace (second (const False) $ current z) z
  if key == KEnter
  then mapM_ (\(tcp, active) -> when active (load file tcp)) deckActives
  else do
    case key of
      KChar ch -> put (FileLoader $ file ++ [ch])
      KBS -> put (FileLoader $ Data.List.init file) -- fix me, loser! dont use init!
      KRight -> maybe (return ()) go (right deckActives)
      KLeft -> maybe (return ()) go (left deckActives)
      KUp -> go $ on deckActives
      KDown -> go $ off deckActives
      _ -> return ()

drawFileLoader :: String -> Zipper (Tcp, Bool) -> App ()
drawFileLoader file deckActives = do
  vty <- (.vty) <$> ask
  let line1 = Vty.string (defAttr `withForeColor` green) file
      deckActivesShow = show $ first tcpHandle <$> deckActives
      line2 = Vty.string (defAttr `withForeColor` red) deckActivesShow
      line3 = Vty.string (defAttr `withForeColor` yellow) "You are in File Loader mode."
  liftIO $ update vty (picForImage $ foldl1 vertJoin [line1,line2,line3])

interpretTreatAsBitstring :: Word256 -> Key -> App ()
interpretTreatAsBitstring w256 key = do
  let line1 = Vty.string (defAttr `withForeColor` blue) (show w256)
      line2 = Vty.string (defAttr `withForeColor` yellow) "You are in Bitstring mode."
  decks <- (.deckSockets) <$> ask
  vty <- (.vty) <$> ask
  liftIO $ update vty (picForImage line1)
  mapM_ (playTracks w256) decks
  case key of
    KChar 'q' -> put (TreatAsBitstring $ complement w256)
    KChar 'a' -> put (TreatAsBitstring $ rotateL w256 1)
    KChar 'd' -> put (TreatAsBitstring $ rotateR w256 1)
    KChar 'A' -> put (TreatAsBitstring $ rotateL w256 10)
    KChar 'D' -> put (TreatAsBitstring $ rotateR w256 10)
    _ -> return ()

interpretQueueBuffer = undefined

interpretInputAsHash :: String -> Key -> App ()
interpretInputAsHash ih key = do
  case key of 
    (KChar ch) -> do
      let msg = "MESSAGE --> " ++ ih ++ pure ch
          adlerDigest = "ADLER-32 HASH --> " ++ (show . adler32 $ BSC8.pack msg)
          msgLine = Vty.string (defAttr `withForeColor` cyan) msg
          adlerLine = Vty.string (defAttr `withForeColor` magenta) (show adlerDigest)
          statsLIne = Vty.string (defAttr `withForeColor` yellow) "You are in Adler-32 mode."
      put $ InputAsHash msg
      decks <- (.deckSockets) <$> ask
      vty <- (.vty) <$> ask
      liftIO $ update vty (picForImage $ vertJoin msgLine adlerLine)
      mapM_ (playTracks adlerDigest) decks
    _ -> return ()

-- "outer" meaning "not needing mode-specific things"
updateOuterDisplay :: OperatingMode -> App ()
updateOuterDisplay = \case
  FileLoader fl -> undefined
  InputAsHash ih -> undefined
  TreatAsBitstring bs -> undefined
  QueueBuffer qb -> undefined


changeModeErrorMessage :: String
changeModeErrorMessage = unlines
  [ "Error: You have an F13 key! Why!?"
  ]


