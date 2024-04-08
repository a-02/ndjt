module Types where

import Control.Monad.Trans.RWS.Strict

import Data.ByteString.Char8 as BSC8
import Data.List.NonEmpty.Zipper
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

data DeckInfo = DeckInfo
  { conn :: Udp
  , active :: Bool
  , home :: HomeDirectory
  }

instance Show DeckInfo where
  show (DeckInfo a b c) = Prelude.unlines
    [ show (udpSocket a)
    , show b
    , show c
    ]

data NDJTArg = NDJTArg
  { argIP :: IP
  , argHome :: HomeDirectory
  , argPort :: Word16
  }

data NDJTState = NDJTState
  { deckSwitches :: Decks
  , mode :: OperatingMode
  }

data NDJTInfo = NDJTInfo
  { vty :: Vty
  , logMainHandle :: Handle
  , logNetworkHandle :: Handle
  , deckSockets :: [Udp]
  }

-- todo: unfuck my nix setup
