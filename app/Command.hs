{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Command where

import Control.Monad.Trans.RWS.Strict

import Colog.Core.Action
import Colog.Core.IO

import Data.Bit
import Data.ByteString.Char8 as BSC8
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU

import Sound.Osc
import Types

start :: Udp -> App ()
start conn = do
  liftIO $ udp_send_packet conn (Packet_Message (message "/renoise/transport/start" []))
  handle <- (.logNetworkHandle) <$> ask
  logStringHandle handle <& ("start playing on whatever deck this is: " ++ show (udpSocket conn))

stop :: Udp -> App ()
stop conn = do
  liftIO $ udp_send_packet conn (Packet_Message (message "/renoise/transport/stop" []))
  handle <- (.logNetworkHandle) <$> ask
  logStringHandle handle <& ("stop playing on whatever deck this is: " ++ show (udpSocket conn))

playTracks :: (Integral a) => a -> Udp -> App ()
playTracks i conn = do
  handle <- (.logNetworkHandle) <$> ask
  let bits = VU.convert $ unBit `VU.map` castFromWords [fromIntegral i]
      mute a = BSC8.pack $ "renoise.song().tracks[" ++ show a ++ "]:mute()"
      unmute a = BSC8.pack $ "renoise.song().tracks[" ++ show a ++ "]:unmute()"
      actions = V.imap (\idx b -> if b then toMessage (unmute idx) else toMessage (mute idx)) bits
  liftIO . udp_send_packet conn . p_bundle immediately $ V.toList actions

toMessage :: BSC8.ByteString -> Message
toMessage luaExpression = message "/renoise/evaluate" [AsciiString luaExpression]

load :: BSC8.ByteString -> Udp -> App ()
load file conn = do
  handle <- (.logNetworkHandle) <$> ask
  let save = "renoise.app():save_song_as(\"/dev/null/lol.xrns\")"
      loadMsg = "renoise.app():load_song(\"" `BSC8.append` file `BSC8.append` "\")"
  logStringHandle handle <& "saving..."
  liftIO $ udp_send_packet conn (Packet_Message $ toMessage save)
  logStringHandle handle <& "save successful"
  logStringHandle handle <& "loading...l"
  liftIO $ udp_send_packet conn (Packet_Message $ toMessage loadMsg)
  logStringHandle handle <& "load successfull"

addScheduledSequence :: [Int] -> Udp -> App ()
addScheduledSequence queue conn = do
  handle <- (.logNetworkHandle) <$> ask
  let msg i = "renoise.song().transport.add_scheduled_sequence(" ++ show i ++ ")"
  mapM_ (\a -> liftIO $ 
    udp_send_packet conn (Packet_Message $ toMessage (BSC8.pack $ msg a))) 
    queue
