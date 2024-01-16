module Types where

import Graphics.Vty
import Sound.Osc
import Control.Monad.Trans.RWS.Strict
import Data.WideWord.Word256
import System.IO
import Data.List.NonEmpty

type App = RWST NDJTInfo () OperatingMode IO

data OperatingMode = 
  FileLoader String | 
  InputAsHash String | 
  TreatAsBitstring Word256 | 
  QueueBuffer [Int]

data NDJTInfo = NDJTInfo {
  vty :: Vty,
  logMainHandle :: Handle,
  logNetworkHandle :: Handle,
  deckSockets :: NonEmpty Tcp
  }

