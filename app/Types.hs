module Types where

import Control.Monad.Trans.RWS.Strict
import Data.ByteString.Char8 as BSC8
import Data.List.NonEmpty
import Data.List.NonEmpty.Zipper
import Data.WideWord.Word256
import Graphics.Vty
import Sound.Osc
import System.IO

type App = RWST NDJTInfo () NDJTState IO

data OperatingMode
  = FileLoader BSC8.ByteString
  | InputAsHash BSC8.ByteString
  | TreatAsBitstring Word256
  | QueueBuffer (Zipper Int)

type Decks = Zipper (Tcp, Bool)

-- todo: implement this everywhere
data NDJTState = NDJTState
  { deckSwitches :: Decks
  , mode :: OperatingMode
  }

data NDJTInfo = NDJTInfo
  { vty :: Vty
  , logMainHandle :: Handle
  , logNetworkHandle :: Handle
  , deckSockets :: NonEmpty Tcp
  }

-- todo: unfuck my nix setup
