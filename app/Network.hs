{-# LANGUAGE OverloadedStrings #-}

module Network where

import Data.Text qualified as T

import Control.Concurrent.Async
import Network.SSH.Client.LibSSH2
import Net.IP

import Util

xrdjPSSHClient :: [T.Text] -> [IP] -> [T.Text] -> IO ()
xrdjPSSHClient as bs cs = mapConcurrently_ (uncurry3 sshSessionXRDJ) (zip3 as bs cs)

sshSessionXRDJ :: T.Text -> IP -> T.Text -> IO ()
sshSessionXRDJ user ip home  =
  withSSH2Agent "~/.ssh/known_hosts" (T.unpack user) (T.unpack . encode $ ip) 22 (sessionAction home ip)

sessionAction :: T.Text -> IP -> Session -> IO ()
sessionAction home ip sesh = do
  let saveFilePath = "temp/digest_" ++ (T.unpack . encode $ ip)
      home' = (T.unpack home ++)
  _ <- scpSendFile sesh 0o777 "xras/target/release/xras" (T.unpack home)
  _ <- runShellCommands sesh ["." ++ home' "/digest"]
  _ <- scpReceiveFile sesh (home' "/out.toml") saveFilePath
  _ <- runShellCommands sesh ["rm " ++ home' "/out.toml", "rm " ++ home' "/digest" ]
  return ()

--todo: dont connect to ssh when speaking to localhost
