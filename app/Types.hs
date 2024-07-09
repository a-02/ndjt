{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Monad.Trans.RWS.Strict

import Data.List.NonEmpty.Zipper
import Data.Text as T
import qualified Data.Map as Map
import Data.Word

import Graphics.Vty

import Net.IP

import Sound.Osc

import System.IO

type App = RWST XRDJInfo () XRDJState IO

data OperatingMode
  = FileLoader
  | Controller
  | Synth
  | Sample
  deriving (Show)

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
  , xrns :: XRNS
  }

data XRDJArg = XRDJArg
  { argUser :: T.Text
  , argIP :: IP
  , argHome :: T.Text
  , argPort :: Word16
  , argText :: T.Text
  } deriving (Show)

type DeckAvailableFiles = Map.Map DeckName [XRNSName]

data XRDJState = XRDJState
  { deckSwitches :: Decks
  , mode :: OperatingMode
  }

data XRDJInfo = XRDJInfo
  { vty :: Vty
  , logMainHandle :: Handle
  , logNetworkHandle :: Handle
  }

-- should be argText from XRDJArg
type DeckName = T.Text 

-- should be name from XRNS
type XRNSName = T.Text

data XRNS = XRNS
  { name :: T.Text
  , bpm :: Float
  , loopCoeff :: Int
  , tracks :: [Track]
  }

data TrackMuteState = Active | Off | Muted
data TrackType = Sequencer | Master | Send | Group
data Track = Track
  { trackName :: T.Text
  , trackType :: TrackType
  , trackMuteState :: TrackMuteState
  , trackColor :: Color 
  }
