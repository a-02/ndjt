{-# LANGUAGE OverloadedStrings #-}

module Network where

import Data.Text qualified as T

import Control.Concurrent.Async
import Network.SSH.Client.LibSSH2
import Net.IP as IP
import Net.IPv4 as V4
import Net.IPv6 as V6

import System.Process

import Util

xrdjPSSHClient :: [T.Text] -> [IP] -> [T.Text] -> IO ()
xrdjPSSHClient as bs cs = mapConcurrently_ (uncurry3 sshSessionXRDJ) (zip3 as bs cs)

sshSessionXRDJ :: T.Text -> IP -> T.Text -> IO ()
sshSessionXRDJ user ip home  =
 if case_ (== V4.localhost) (== V6.localhost) ip
 then undefined
 else withSSH2Agent "~/.ssh/known_hosts" (T.unpack user) (T.unpack . IP.encode $ ip) 22 (sessionAction home ip)

sessionAction :: T.Text -> IP -> Session -> IO ()
sessionAction home ip sesh = do
  let saveFilePath = "/tmp/xrdj/rpfps_" ++ (T.unpack . IP.encode $ ip)
      home' = (T.unpack home ++)
  _ <- scpSendFile sesh 0o777 "rpfps/target/release/rpfps" (T.unpack home)
  _ <- runShellCommands sesh ["." ++ home' "/digest"]
  _ <- scpReceiveFile sesh (home' "/out.toml") saveFilePath
  _ <- runShellCommands sesh ["rm " ++ home' "/out.toml", "rm " ++ home' "/rpfps" ]
  return ()

--todo: dont connect to ssh when speaking to localhost
