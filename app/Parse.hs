{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Net.IP
import Net.IPv4 as V4
import Net.IPv6 as V6
import Data.Attoparsec.Text as AT
import Control.Applicative 
import Colog.Core.Action
import Data.Word

import Data.Text qualified as T
import Data.Text.Encoding as TE
import Data.ByteString.Char8 as BSC8

import System.IO

import Logging
import Types

parseNDJTArg :: Parser NDJTArg
parseNDJTArg = do
  (argIP :: IP) <- (fromIPv4 <$> V4.parser) <|> (fromIPv6 <$> V6.parser)
  skip (== ':')
  (argPort :: Word16) <- decimal
  skip (== ':')
  (argHome :: BSC8.ByteString) <- TE.encodeUtf8 <$> takeText
  (argText :: T.Text) <- return ""
  return NDJTArg{..}

processArgs :: Handle -> [BSC8.ByteString] -> IO [NDJTArg]
processArgs handle bs = do
  let txs = TE.decodeUtf8 <$> bs
  logByteStringLn handle <& "consuming arguments"
  mapM parseWithError txs
    where parseWithError :: T.Text -> IO NDJTArg
          parseWithError tx = case AT.parseOnly (parseNDJTArg <* endOfInput) tx of
            Right r -> return (r {argText = tx})
            Left e -> do logByteStringLn handle <& failParseError e; error "failed to parse, check logs"

partialParseError :: ByteString
partialParseError = 
  "command line parsing failed, returned Partial."

failParseError :: String -> ByteString
failParseError e =
  "command line parsing failed, returned Fail: " `BSC8.append` BSC8.pack e
