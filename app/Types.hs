{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

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

type App = RWST NDJTInfo () NDJTState IO

type HomeDirectory = BSC8.ByteString

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
      f  = (.arg.argText)
   in T.concat
        [ T.unlines $ f <$> l
        , f di `T.append` " <---\n"
        , T.unlines $ f <$> r
        ]

data DeckInfo = DeckInfo
  { conn :: Udp
  , active :: Bool
  , home :: HomeDirectory
  , arg :: NDJTArg
  }

data NDJTArg = NDJTArg
  { argIP :: IP
  , argHome :: HomeDirectory
  , argPort :: Word16
  , argText :: T.Text
  } deriving (Show)

data NDJTState = NDJTState
  { deckSwitches :: Decks
  , mode :: OperatingMode
  }

data NDJTInfo = NDJTInfo
  { vty :: Vty
  , logMainHandle :: Handle
  , logNetworkHandle :: Handle
  }

-- todo: unfuck my nix setup
