module Logging where

import Colog.Core.Action
import Control.Monad.IO.Class
import Data.ByteString
import System.IO (Handle)

logByteStringLn :: MonadIO m => Handle -> LogAction m ByteString
logByteStringLn handle = LogAction $ liftIO . hPut handle . (`snoc` 10) -- fromEnum '\n' = 10
{-# INLINE logByteStringLn #-}
{-# SPECIALIZE logByteStringLn :: Handle -> LogAction IO ByteString #-}
