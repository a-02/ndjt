{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Draw where

import Control.Monad.IO.Class
import Control.Monad.Trans.RWS.Strict
import Graphics.Vty as Vty
import Data.ByteString.Char8 qualified as BSC8
import Data.Digest.Adler32 -- this is temporary, carry the digest with the opmode, loser
import Data.Text qualified as T
import Data.WideWord.Word256
import Data.List.NonEmpty.Zipper as Z
import Sound.Osc

import Types
import Util

drawFileLoader :: App ()
drawFileLoader = do
  vty <- (.vty) <$> ask
  st <- get
  let textBlock = T.unlines
        [ "You are in File Loader mode."
        , T.pack $ showOperatingMode st.mode
        , showDecks st.deckSwitches
        ]
  liftIO $ update vty (picForImage $ colorImage primaryColors textBlock)

drawSample :: App ()
drawSample = undefined

drawController :: App ()
drawController = undefined

drawSynth :: App ()
drawSynth = undefined

drawLanding :: Vty -> [T.Text] -> IO ()
drawLanding vty decks = do
  update vty (picForImage $ colorImage otherColors landingText)
 where
  landingText = block1 `T.append` deckText `T.append` block2
  block1 = T.unlines
    [ "xrdj: xrns remote disc jockey"
    , "v.0.0.0.0"
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
    , "F5: Controller mode. You're in control now."
    , "ESC: Close NDJT."
    ]

colorImage :: [Color] -> T.Text -> Image
colorImage colors tx =
  foldl1 vertJoin $
    (\(a, b) -> Vty.text' (defAttr `withForeColor` b) a) <$> zip (T.lines tx) colors

themeColors :: [Color]
themeColors =
  let u3 f (a, b, c) = f a b c
   in u3 linearColor <$> Prelude.reverse [(i, j, k) | (i :: Int) <- [0, 64 .. 255], j <- [255, 224 .. 0], k <- [0, 64 .. 255]]

primaryColors :: [Color]
primaryColors = cycle [red, yellow, green, blue]

otherColors :: [Color]
otherColors = cycle [cyan, magenta, white]

