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
import Data.Foldable
import Data.Function hiding (on)
import Data.List.NonEmpty.Zipper as Z
import Data.Time
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import GHC.Bits

import Graphics.Vty as Vty
import Graphics.Vty.Platform.Unix (mkVty)

import Net.IP as IP

import Sound.Osc

import System.Directory
import System.Exit
import System.IO
import System.Posix.Env.ByteString
import Network (xrdjPSSHClient)

main :: IO ()
main = do
  -- grabbing eveyrthing

  (args :: [BSC8.ByteString])  <- getArgs
  rightNow <- getCurrentTime
  let now = formatTime defaultTimeLocale "%T" rightNow
      filename = "log/xrdj_" ++ now
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

  let users = argUser <$> argsRes
      ports = fromIntegral . argPort <$> argsRes
      ips = argIP <$> argsRes
      decks = IP.encode <$> ips
      homeDirectories = argHome <$> argsRes
      argsTextRep = argText <$> argsRes

  xrdjPSSHClient users ips homeDirectories
  
  xrnss <- readXRNSFromTmp

  deckSockets <- zipWithM openUdp (T.unpack <$> decks) ports
  logByteStringLn logNetworkHandle <& "created deck sockets"

  -- setting up initial state and read-only constants

  drawLanding vty argsTextRep
  let info = XRDJInfo{vty, logMainHandle, logNetworkHandle}
      activeDecks = listToZipper . getZipList $ 
        DeckInfo 
          <$> ZipList deckSockets 
          <*> ZipList (replicate (length homeDirectories) False) 
          <*> ZipList homeDirectories 
          <*> ZipList argsRes
          <*> ZipList xrnss
          -- pissing me off.
      st = XRDJState{deckSwitches = activeDecks, mode = FileLoader}

  -- run main loop, exit when done

  logTextLn logMainHandle <& T.unlines homeDirectories
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
    FileLoader -> interpretFileLoader
    Controller -> interpretController
    Synth -> interpretSynth
    Sample -> interpretSample     

changeMode :: Int -> App ()
changeMode i = do 
    vty <- (.vty) <$> ask
    handle <- (.logMainHandle) <$> ask -- Woah!
    st <- get
    let decks = deckSwitches st
    case i of
      1 -> do
        logByteStringLn handle <& "switching to FileLoader"
        put $ st{mode = FileLoader}
        drawFileLoader
      2 -> do
        logByteStringLn handle <& "switching to Controller"
        put $ st{mode = Controller}
        drawController
      3 -> do
        logByteStringLn handle <& "switching to Synth"
        put $ st{mode = Synth}
        drawSynth
      4 -> do
        logByteStringLn handle <& "switching to Sample"
        put $ st{mode = Sample}
        drawSample
      _ -> do
        logStringHandle handle <& changeModeErrorMessage
        liftIO exitFailure

interpretFileLoader :: Key -> App ()
interpretFileLoader key = do
  handle <- (.logMainHandle) <$> ask
  st <- get
  drawFileLoader

interpretController :: Key -> App ()
interpretController = undefined

interpretSynth :: Key -> App ()
interpretSynth = undefined

interpretSample :: Key -> App ()
interpretSample = undefined

interpretArrows :: Key -> App ()
interpretArrows key = do 
  st <- get 
  case key of
    KRight -> maybe (return ()) (\x -> put $ st { deckSwitches = x }) (right st.deckSwitches)
    KLeft -> maybe (return ()) (\x -> put $ st { deckSwitches = x }) (left st.deckSwitches)
    KUp -> put $ st { deckSwitches = on st.deckSwitches }
    KDown -> put $ st { deckSwitches = off st.deckSwitches }
    _ -> return ()

changeModeErrorMessage :: String
changeModeErrorMessage =
  unlines
    [ "Error: You have an F13 key! Why!?"
    ]
