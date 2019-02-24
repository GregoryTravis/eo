module Resample (resample) where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import System.Directory
import System.Exit
import System.Process

import Util

cacheDir = "loop_cache"

getLength filename = do
  --(a, _, stderr') <- readProcessWithExitCode "/usr/local/bin/sox" [filename, "-n", "stat"] ""
  --msp ("mspa", a)
  --msp stderr'
  (ExitSuccess, _, stderr) <- readProcessWithExitCode "/usr/local/bin/sox" [filename, "-n", "stat"] ""
  let ws = words stderr
  return $ assert (take 2 ws == ["Samples", "read:"])
    ((read $ ws !! 2) :: Integer)

-- Double the speed ratio until it's >= 0.5
notTooSlow x
  | 0 < x && x < 0.5 = notTooSlow (x * 2)
  | otherwise = x

md5File filename = do
  bs <- BS.readFile filename
  return $ MD5.finalize $ MD5.update MD5.init bs

resample src destLengthFrames = do
  srcLengthFrames <- getLength src
  hash <- md5File src
  let dest = cacheDir ++ "/_" ++ (C8.unpack $ B16.encode hash) ++ "_" ++ (show destLengthFrames) ++ ".wav"
  exists <- doesFileExist dest
  if not exists
    then do let speedRatio = notTooSlow $ (fromIntegral srcLengthFrames) / (fromIntegral destLengthFrames)
            callProcess "/usr/local/bin/sox" [src, dest, "speed", show speedRatio]
    else do -- msp ("cache hit", dest)
            return ()
  return dest
