{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NegativeLiterals #-}

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
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty.Zipper as Z
import Data.Time
import Data.WideWord.Word256

import Graphics.Vty as Vty
import GHC.List hiding (foldl1)

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
collapseEventToKey a = Left $ "got " ++ (show a)

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
    QueueBuffer qb -> interpretQueueBuffer qb fileLoaderActiveDecks

changeMode :: Int -> App ()
changeMode = \case
  1 -> put $ FileLoader []
  2 -> put $ InputAsHash []
  3 -> put $ TreatAsBitstring zeroWord256
  4 -> put . QueueBuffer $ listToZipper [0]
  5 -> put . QueueBuffer $ listToZipper [0]
  6 -> put . QueueBuffer $ listToZipper [0]
  7 -> put . QueueBuffer $ listToZipper [0]
  8 -> put . QueueBuffer $ listToZipper [0]
  9 -> put . QueueBuffer $ listToZipper [0]
  10 -> put . QueueBuffer $ listToZipper [0]
  11 -> put . QueueBuffer $ listToZipper [0]
  12 -> put . QueueBuffer $ listToZipper [0]
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
  if key == KEnter
  then mapM_ (\(tcp, active) -> when active (load file tcp)) deckActives
  else do
    case key of
      KChar ch -> put (FileLoader $ file ++ [ch])
      KBS -> maybe (return ()) (put . FileLoader . BSC8.unpack . fst) (BSC8.unsnoc . BSC8.pack $ file) -- ugh
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
  liftIO $ update vty (picForImage $ foldl1 vertJoin [line1,line2])
  mapM_ (playTracks w256) decks
  case key of
    KChar 'q' -> put (TreatAsBitstring $ complement w256)
    KChar 'a' -> put (TreatAsBitstring $ rotateL w256 1)
    KChar 'd' -> put (TreatAsBitstring $ rotateR w256 1)
    KChar 'A' -> put (TreatAsBitstring $ rotateL w256 10)
    KChar 'D' -> put (TreatAsBitstring $ rotateR w256 10)
    _ -> return ()

interpretQueueBuffer :: Zipper Int -> Zipper (Tcp, Bool) -> Key -> App ()
interpretQueueBuffer qb decks key = do
  vty <- (.vty) <$> ask
  let go qb' decks' = liftIO (either error id . collapseEventToKey <$> nextEvent vty) >>= interpretQueueBuffer qb' decks'
  case key of
    KRight -> maybe (return ()) (go qb) (right decks)
    KLeft -> maybe (return ()) (go qb) (left decks)
    KUp -> go qb $ on decks
    KDown -> go qb $ off decks
    KChar 'w' -> put (QueueBuffer $ add 1 qb)
    KChar 's' -> put (QueueBuffer $ add -1 qb)
    KChar 'a' -> maybe (return ()) (put . QueueBuffer) (left qb)
    KChar 'd' -> maybe (return ()) (put . QueueBuffer) (right qb)
    KChar 'W' -> put (QueueBuffer $ add 10 qb)
    KChar 'S' -> put (QueueBuffer $ add -10 qb)
    _ -> return ()

interpretInputAsHash :: String -> Key -> App ()
interpretInputAsHash ih key = do
  case key of 
    (KChar ch) -> do
      let msg = "MESSAGE --> " ++ ih ++ pure ch
          adler = adler32 $ BSC8.pack msg
          adlerDigest = "ADLER-32 HASH --> " ++ show adler
          msgLine = Vty.string (defAttr `withForeColor` cyan) msg
          adlerLine = Vty.string (defAttr `withForeColor` magenta) (show adlerDigest)
          statsLine = Vty.string (defAttr `withForeColor` yellow) "You are in Adler-32 mode."
      put $ InputAsHash msg
      decks <- (.deckSockets) <$> ask
      vty <- (.vty) <$> ask
      liftIO $ update vty (picForImage $ foldl1 vertJoin [msgLine,adlerLine,statsLine])
      mapM_ (playTracks adler) decks
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

listToZipper :: [a] -> Z.Zipper a
listToZipper = Z.fromNonEmpty . NE.fromList

on :: Bifunctor f => Zipper (f a Bool) -> Zipper (f a Bool)
on z = replace (second (const True) $ current z) z

add :: Int -> Zipper Int -> Zipper Int
add x z = replace ((+ x) . current $ z) z

off :: Bifunctor f => Zipper (f a Bool) -> Zipper (f a Bool)
off z = replace (second (const False) $ current z) z

{--
instance Monoid a => Monoid (Zipper a) where
  mempty = Z.fromNonEmpty . NE.fromList $ [mempty]
  mappend = (<>) -- oh fuck i need to find out if Zipper is an applicative

instance Semigroup a => Semigroup (Zipper a) where
(<>) = undefined -- ah hell
--}
