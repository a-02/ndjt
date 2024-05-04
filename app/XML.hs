{-# LANGUAGE RecordWildCards #-}

module XML where

import System.Process

import Types

-- There is no real way for a BPM to be anything except a float.
-- No matter how you feel about "read" being partial.
--
-- Similarly, loopCoeff will always be an integer 2-16.
-- Would've loved to implement this as a u4 in Rust, but whatever.
readXRNS :: FilePath -> IO XRNS
readXRNS path = do
  out <- readProcess "ndjt-xml-rust/target/release/ndjt-xml-rust" [path] []
  let res = lines out
      name = read $ head res
      bpm = read $ res !! 1
      loopCoeff = read $ res !! 2
  return XRNS{..} 
      

