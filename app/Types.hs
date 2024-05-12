{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Control.Monad.Trans.RWS.Strict

import Data.ByteString.Char8 as BSC8
import Data.List.NonEmpty.Zipper
import Data.Text as T
import Data.WideWord.Word256
import Data.Word

import Graphics.Vty

import Net.IP

import Sound.Osc

import System.IO

type App = RWST XRDJInfo () XRDJState IO

data OperatingMode
  = FileLoader BSC8.ByteString
  | InputAsHash BSC8.ByteString
  | TreatAsBitstring Word256
  | QueueBuffer (Zipper Int)

type Decks = Zipper DeckInfo

showDecks :: Decks -> T.Text
showDecks decks = 
  let di = current decks
      l  = lefts decks
      r  = rights decks
      g x = x.arg.argText `T.append` deckBool x.active
   in T.concat
        [ T.unlines $ g <$> l
        , g di `T.append` " <---\n"
        , T.unlines $ g <$> r
        ]

deckBool :: Bool -> T.Text
deckBool True = " On "
deckBool False = " Off "

data DeckInfo = DeckInfo
  { conn :: Udp
  , active :: Bool
  , home :: T.Text
  , arg :: XRDJArg
  }

data XRDJArg = XRDJArg
  { argUser :: T.Text
  , argIP :: IP
  , argHome :: T.Text
  , argPort :: Word16
  , argText :: T.Text
  } deriving (Show)

data XRDJState = XRDJState
  { deckSwitches :: Decks
  , mode :: OperatingMode
  }

data XRDJInfo = XRDJInfo
  { vty :: Vty
  , logMainHandle :: Handle
  , logNetworkHandle :: Handle
  }

data XRNS = XRNS
  { name :: T.Text
  , bpm :: Float
  , loopCoeff :: Int
  }
