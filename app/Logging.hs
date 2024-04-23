module Logging where

import Colog.Core.Action
import Control.Monad.IO.Class
import Data.ByteString
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.IO (Handle)

logByteStringLn :: MonadIO m => Handle -> LogAction m ByteString
logByteStringLn handle = LogAction $ liftIO . hPut handle . (`snoc` 10) -- fromEnum '\n' = 10
{-# INLINE logByteStringLn #-}
{-# SPECIALIZE logByteStringLn :: Handle -> LogAction IO ByteString #-}

logTextLn :: MonadIO m => Handle -> LogAction m T.Text
logTextLn handle = LogAction $ liftIO . T.hPutStrLn handle
{-# INLINE logTextLn #-}
{-# SPECIALIZE logTextLn :: Handle -> LogAction IO T.Text #-}
