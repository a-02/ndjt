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
import Data.Foldable
import Data.Function hiding (on)
import Data.Digest.Adler32
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
  drawLanding vty decks
  logStringHandle logNetworkHandle <& "successfully got deckSockets?"
  let info = NDJTInfo {vty, logMainHandle, logNetworkHandle, deckSockets}
  _ <- execRWST vtyGO info (FileLoader "")
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
  decks <- (.deckSockets) <$> ask
  let activeDecks = fromNonEmpty . NE.zip decks $ fix (False NE.<|)
  case key of
    (KFun i) -> changeMode i activeDecks
    _ -> modeBranch key activeDecks

modeBranch :: Key -> Zipper (Tcp, Bool) -> App ()
modeBranch key decks = do
  mode <- get
  key & case mode of
    FileLoader fl -> interpretFileLoader fl decks
    InputAsHash ih -> interpretInputAsHash ih
    TreatAsBitstring bs -> interpretTreatAsBitstring bs
    QueueBuffer qb -> interpretQueueBuffer qb decks

changeMode :: Int -> Zipper (Tcp, Bool) -> App ()
changeMode i decks = 
  do vty <- (.vty) <$> ask 
     case i of
          1 -> do 
            put $ FileLoader []
            drawFileLoader [] decks
          2 -> do 
            put $ InputAsHash []
            drawInputHash [] []
          3 -> do
            put $ TreatAsBitstring zeroWord256
            drawBitstring zeroWord256
          4 -> do
            put . QueueBuffer $ listToZipper [0]
            drawQueueBuffer (listToZipper [0]) decks
          5 -> do 
            put $ FileLoader []
            drawFileLoader [] decks
          6 -> do 
            put $ InputAsHash []
            drawInputHash [] [] 
          7 -> do
            put $ TreatAsBitstring zeroWord256
            drawBitstring zeroWord256
          8 -> do
            put . QueueBuffer $ listToZipper [0]
            drawQueueBuffer (listToZipper [0]) decks
          9 -> do 
            put $ FileLoader []
            drawFileLoader [] decks
          10 -> do 
            put $ InputAsHash []
            drawInputHash [] []
          11 -> do
            put $ TreatAsBitstring zeroWord256
            drawBitstring zeroWord256 
          12 -> do
            put . QueueBuffer $ listToZipper [0]
            drawQueueBuffer (listToZipper [0]) decks
          _   -> do handle <- (.logMainHandle) <$> ask -- Woah!
                    logStringHandle handle <& changeModeErrorMessage
                    liftIO exitFailure

interpretFileLoader :: String -> Zipper (Tcp, Bool) -> Key -> App ()
interpretFileLoader file deckActives key = do
  vty <- (.vty) <$> ask
  let go x = do 
        k <- liftIO $ collapseEventToKey <$> nextEvent vty
        interpretFileLoader file x (either error id k) -- oops! got lazy
  if key == KEnter
  then mapM_ (\(tcp, active) -> when active (load file tcp)) deckActives
  else do
    case key of
      KChar ch -> do
        drawFileLoader (file ++ [ch]) deckActives
        put (FileLoader $ file ++ [ch])
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

drawLanding :: Vty -> NE.NonEmpty String -> IO ()
drawLanding vty decks = do
  update vty (picForImage $ rainbowImage landingText)
  where landingText = 
            [ "Welcome to the NKS Renoise Multitool!"
            , "(c) Regular Normal SoftWorks, 2023-2024"
            , "755 W. 32nd St. Chicago, IL 60609"
            , ""
            , "You are connected to:"
            ] ++ NE.toList decks ++
            [ ""
            , "F1: File Loader mode. Use this to load XRNS files into NDJT."
            , "F2: Adler-32 mode. Control track solos with Adler-32 hash."
            , "F3: Bitstring mode. Control track solos as if it were a bitstring."
            , "F4: Queue Buffer mode. Dedicated sequence scheduler."
            , "ESC: Close NDJT."
            ]

rainbowImage :: [String] -> Image
rainbowImage strings =
  foldl1 vertJoin $ 
    (\(a,b) -> Vty.string (defAttr `withForeColor` b) a) <$> zip strings allColors

allColors :: [Color]
allColors = let u3 f (a,b,c) = f a b c in
  u3 linearColor <$> Prelude.reverse [ (i,j,k) | (i :: Int) <- [0,64..255], j <- [255,224..0], k <- [255,224..0] ]
  -- holy shit whens the last time i used a list comprehension?

interpretTreatAsBitstring :: Word256 -> Key -> App ()
interpretTreatAsBitstring w256 key = do
  decks <- (.deckSockets) <$> ask
  drawBitstring w256
  mapM_ (playTracks w256) decks
  case key of
    KChar 'q' -> put (TreatAsBitstring $ complement w256)
    KChar 'a' -> put (TreatAsBitstring $ rotateL w256 1)
    KChar 'd' -> put (TreatAsBitstring $ rotateR w256 1)
    KChar 'A' -> put (TreatAsBitstring $ rotateL w256 10)
    KChar 'D' -> put (TreatAsBitstring $ rotateR w256 10)
    _ -> return ()

drawBitstring :: Word256 -> App ()
drawBitstring w256 = do
  let line1 = Vty.string (defAttr `withForeColor` blue) (show w256)
      line2 = Vty.string (defAttr `withForeColor` yellow) "You are in Bitstring mode."
  vty <- (.vty) <$> ask
  liftIO $ update vty (picForImage $ foldl1 vertJoin [line1,line2])
  

interpretQueueBuffer :: Zipper Int -> Zipper (Tcp, Bool) -> Key -> App ()
interpretQueueBuffer qb decks key = do
  vty <- (.vty) <$> ask
  let go qb' decks' = liftIO (either error id . collapseEventToKey <$> nextEvent vty) >>= interpretQueueBuffer qb' decks'
  case key of
    KEnter -> mapM_ (\(tcp, active) -> when active (addScheduledSequence (toList qb) tcp)) decks
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

drawQueueBuffer :: Zipper Int -> Zipper (Tcp, Bool) -> App ()
drawQueueBuffer qb decks = do
  vty <- (.vty) <$> ask
  let line1 = Vty.string (defAttr `withForeColor` cyan) (show qb)
      line2 = Vty.string (defAttr `withForeColor` magenta) (show $ first tcpHandle <$> decks)
      line3 = Vty.string (defAttr `withForeColor` yellow) "You are in Queue Buffer mode."
  liftIO $ update vty (picForImage $ foldl1 vertJoin [line1,line2,line3])

interpretInputAsHash :: String -> Key -> App ()
interpretInputAsHash ih key = do
  let msg c = "MESSAGE --> " ++ ih ++ c
      adler c = adler32 $ BSC8.pack (ih ++ c)
      adlerDigest = "ADLER-32 HASH --> " ++ show (adler [])
  drawInputHash (msg []) adlerDigest 
  case key of 
    (KChar ch) -> do
      put $ InputAsHash (msg [ch])
      decks <- (.deckSockets) <$> ask
      mapM_ (playTracks $ adler [ch]) decks
    _ -> return ()

drawInputHash :: String -> String -> App ()
drawInputHash msg adler = do
  vty <- (.vty) <$> ask
  let msgLine = Vty.string (defAttr `withForeColor` cyan) msg
      adlerLine = Vty.string (defAttr `withForeColor` magenta) (show adler)
      statsLine = Vty.string (defAttr `withForeColor` yellow) "You are in Adler-32 mode."
  liftIO $ update vty (picForImage $ foldl1 vertJoin [msgLine,adlerLine,statsLine])

{-
-- "outer" meaning "not needing mode-specific things"
updateOuterDisplay :: OperatingMode -> App ()
updateOuterDisplay = \case
  FileLoader fl -> undefined
  InputAsHash ih -> undefined
  TreatAsBitstring bs -> undefined
  QueueBuffer qb -> undefined
-}


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
