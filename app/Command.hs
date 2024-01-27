{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Command where

import Control.Monad.Trans.RWS.Strict

import Colog.Core.Action
import Colog.Core.IO

import Data.Bit
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Word
import Data.WideWord.Word256

import Types
import Sound.Osc


start :: Tcp -> App ()
start conn = do
  liftIO $ tcp_send_packet conn (Packet_Message (message "/renoise/transport/start" []))
  handle <- (.logNetworkHandle) <$> ask
  logStringHandle handle <& ("start playing on whatever deck this is: " ++ show (tcpHandle conn))

stop :: Tcp -> App ()
stop conn = do
  liftIO $ tcp_send_packet conn (Packet_Message (message "/renoise/transport/stop" []))
  handle <- (.logNetworkHandle) <$> ask
  logStringHandle handle <& ("stop playing on whatever deck this is: " ++ show (tcpHandle conn))

playTracks :: Integral a => a -> Tcp -> App ()
playTracks i conn = do
  let bits = VU.convert $ unBit `VU.map` castFromWords [fromIntegral i]
      mute a = "renoise.song().tracks[" ++ show a ++ "]:mute()"
      unmute a = "renoise.song().tracks[" ++ show a ++ "]:unmute()"
      actions = V.imap (\idx b -> if b then toMessage (unmute idx) else toMessage (mute idx)) bits
  liftIO . tcp_send_packet conn . p_bundle immediately $ V.toList actions

toMessage :: String -> Message
toMessage luaExpression = message ("/renoise/evaluate" ++ luaExpression) []

load :: String -> Tcp -> App ()
load file conn = do
  let save = "renoise.app():save_song_as(/dev/null/lol.xrns)"
      loadMsg = "renoise.app():load_song(" ++ file ++ ")"
  liftIO $ tcp_send_packet conn (Packet_Message (message save []))
  liftIO $ tcp_send_packet conn (Packet_Message (message loadMsg []))

addScheduledSequence :: Int -> Tcp -> App ()
addScheduledSequence pttrn conn = do
  let msg = "renoise.song().transport.add_scheduled_sequence(" ++ show pttrn ++ ")"
  liftIO $ tcp_send_packet conn (Packet_Message $ toMessage msg)
