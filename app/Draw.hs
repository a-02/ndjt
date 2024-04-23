{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Draw where

import Control.Monad.IO.Class
import Control.Monad.Trans.RWS.Strict
import Graphics.Vty as Vty
import Data.ByteString.Char8 qualified as BSC8
import Data.Text qualified as T
import Data.Text.Encoding as TE
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
        [ T.pack $ fromFileLoader st.mode
        , showDecks st.deckSwitches
        , "You are in File Loader mode."
        ]
  liftIO $ update vty (picForImage $ colorImage primaryColors textBlock)

drawBitstring :: Word256 -> App ()
drawBitstring w256 = do
  let textBlock = T.unlines
        [ T.pack $ show w256
        , "You are in Bitstring mode."
        ]
  vty <- (.vty) <$> ask
  liftIO $ update vty (picForImage $ colorImage (hungryRotate 1 primaryColors) textBlock)

drawQueueBuffer :: Zipper Int -> Decks -> App ()
drawQueueBuffer qb decks = do
  vty <- (.vty) <$> ask
  let textBlock = T.unlines
        [ T.pack $ show qb
        , T.pack . show $ (\DeckInfo{..} -> udpSocket conn) <$> decks
        , T.pack "You are in Queue Buffer mode."
        ]
  liftIO $ update vty (picForImage $ colorImage (hungryRotate 2 primaryColors) textBlock)

drawInputHash :: BSC8.ByteString -> BSC8.ByteString -> App ()
drawInputHash msg adler = do
  vty <- (.vty) <$> ask
  let textBlock = T.unlines
        [ TE.decodeUtf8 msg
        , TE.decodeUtf8 adler
        , "You are in Adler-32 mode."
        ]
  liftIO $ update vty (picForImage $ colorImage (hungryRotate 3 primaryColors) textBlock)

drawLanding :: Vty -> [T.Text] -> IO ()
drawLanding vty decks = do
  update vty (picForImage $ colorImage otherColors landingText)
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
primaryColors = [red, yellow, green, blue]

otherColors :: [Color]
otherColors = [cyan, magenta, white]

