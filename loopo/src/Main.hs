{-# LANGUAGE ScopedTypeVariables #-}
import Control.Concurrent (threadDelay)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as S
--import qualified Data.Vector as V
import qualified Data.StorableVector as SV
import Data.StorableVector.Base as SVB
import Foreign.C
import Foreign.Marshal.Array (mallocArray, copyArray)
--import Foreign.Marshal.Utils (fillBytes)
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable (Storable)
import Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Process

import Util

foreign import ccall "foo" foo :: CInt -> IO CInt
foreign import ccall "bar" bar :: Ptr CFloat -> IO ()

foreign import ccall "init_audio" init_audio :: IO ()
foreign import ccall "write_audio" write_audio :: Ptr Float -> Int -> IO ()
foreign import ccall "term_audio" term_audio :: IO ()

theBufferSize = 64
desiredLengthFrames = 44100 * 2
lowKey = 36

doubleIt [] = []
doubleIt (x : xs) = x : x : (doubleIt xs)

copyAndStereoize :: Storable a => Int -> Vector a -> Vector a
copyAndStereoize 1 v = SV.pack (doubleIt (SV.unpack v))
copyAndStereoize 2 v = v

-- Concatenate enough copies of the vector to reach the standard length; must
-- be an integral number of copies.
maybeLoop :: Storable a => SV.Vector a -> SV.Vector a
maybeLoop v =
  let (numCopies, 0) = divMod desiredLengthFrames (SV.length v)
   in SV.concat (replicate numCopies v)

gruu :: String -> IO (Vector Float)
gruu filename = do
  -- open the file that we want to know about
  --f <- SF.openFile "loop2.wav" SF.ReadMode SF.defaultInfo

  -- read the information about the file out
  --let info = SF.hInfo f

  --bruf :: BV.Buffer CFloat
  (info, Just (bruf :: BV.Buffer Float)) <- SF.readFile filename
  let v' = BV.fromBuffer bruf
  msp ("qqq before", SV.length v')
  let v = maybeLoop $ v'
  msp ("qqq after", SV.length v)

  -- display information about the file
  putStrLn $ "format:      " ++ (show $ SF.format info)
  putStrLn $ "sample rate: " ++ (show $ SF.samplerate info)
  putStrLn $ "channels:    " ++ (show $ SF.channels info)
  putStrLn $ "frames:      " ++ (show $ SF.frames info)

  --assertM "huhh" (a == 0) ()
  let stereoV = copyAndStereoize (SF.channels info) v
  return stereoV

writeAudioAllAtOnce :: Int -> Ptr Float -> IO ()
writeAudioAllAtOnce bufferSize buffer = do write_audio buffer bufferSize

writeAudioAllAtOnce' :: Vector Float -> IO ()
writeAudioAllAtOnce' v =
  --let (fp, start, length) = SVB.toForeignPtr v
      --aFloat = (undefined :: Float)
   --in withForeignPtr fp (\ptr -> writeAudioAllAtOnce length (plusPtr ptr (start * 2 * (sizeOf aFloat))))
  let (fp, 0, length) = SVB.toForeignPtr v
   in withForeignPtr fp (\ptr -> writeAudioAllAtOnce (length `div` 2) ptr)

atoi s = read s :: Int
noteOffsets = Map.fromList [('A', 9), ('B', 11), ('C', 0), ('D', 2), ('E', 4), ('F', 5), ('G', 7)]
--offsetNotes = Map.fromList $ map (\pr -> case pr of (c, o) -> (o, c)) (Map.toList noteOffsets)
--whiteKeys = [0, 2, 4, 5, 7, 9, 11]
--pitchToWhiteKey p = fromJust $ find (p ==) whiteKeys

-- parsePitch :: [Char] -> Int
parsePitch [letter, octave] = ((atoi [octave] + 1) * 12) + fromJust (Map.lookup letter noteOffsets) -- ((ord letter) - (ord 'A') - 2)
parsePitch [letter, sharp, octave] = parsePitch [letter, octave] + 1

parseEvent :: String -> Maybe (Int, Bool)
parseEvent line =
  case (words line) of
    [channel, one, eventType, pitchS, velocityS] ->
      assert (channel == "channel")
        assert (one == "1")
          assert (eventType == "note-on" || eventType == "note-off")
            Just (parsePitch pitchS, eventType == "note-on")
    _ -> Nothing

processEvents :: S.Set Int -> IO (S.Set Int)
processEvents downKeys = do
  ready <- hReady stdin
  if ready then do line <- getLine
                   --msp line
                   --let (n, isDown) = parseEvent line
                   let newDownKeys = case parseEvent line of Just (n, isDown) -> if isDown then S.insert n downKeys else S.delete n downKeys
                                                             Nothing -> downKeys
                   processEvents newDownKeys
           else return downKeys

getActiveSamples :: [a] -> S.Set Int -> [a]
getActiveSamples loops active =
  map (\(_, loop) -> loop) $ filter (\(i, loop) -> isActive i) (zip [0..] loops)
  where isActive i = S.member (i + lowKey) active

-- mixBuffers resampled buffer newDownKeys
mixBuffers' :: [Vector Float] -> Int -> S.Set Int -> Vector Float
mixBuffers' loops curPos downKeys =
  let remaining = desiredLengthFrames - curPos
      toWrite = min remaining theBufferSize
      activeLoops = getActiveSamples loops downKeys
   in SV.sample (toWrite * 2) (mixSamples activeLoops)
  where mixSamples :: [Vector Float] -> Int -> Float
        mixSamples loops i = sum $ map (\loop -> (SV.index loop ((curPos * 2) + i))) loops

getLength filename = do
  --(a, _, stderr') <- readProcessWithExitCode "/usr/local/bin/sox" [filename, "-n", "stat"] ""
  --msp ("mspa", a)
  --msp stderr'
  (ExitSuccess, _, stderr) <- readProcessWithExitCode "/usr/local/bin/sox" [filename, "-n", "stat"] ""
  msp "ughgh"
  msp stderr
  msp "ughgh"
  let ws = eesp "ugh" $ words stderr
  return $ assert (take 2 ws == ["Samples", "read:"])
    ((read $ ws !! 2) :: Integer)

-- Double the speed ratio until it's >= 0.5
notTooSlow x
  | 0 < x && x < 0.5 = notTooSlow (x * 2)
  | otherwise = x

resample src = do
  srcLengthFrames <- getLength src
  let dest = "_" ++ src
  let speedRatio = notTooSlow $ (fromIntegral srcLengthFrames) / (fromIntegral desiredLengthFrames)
  msp ("rah", src, speedRatio)
  callProcess "/usr/local/bin/sox" [src, dest, "speed", show speedRatio]
  return dest

pressDiagram numSamples keys = "[" ++ map onOff [0..numSamples-1] ++ "]"
  where onOff i = if (S.member (i+lowKey) keys) then '#' else '.'

main = do hSetBuffering stdout NoBuffering
          putStrLn "asdf"
          i <- foo 12
          putStrLn "asdf2"
          args <- getArgs
          msp "args"
          msp args

          let downKeys = S.empty

          init_audio
          putStrLn (show i)

          resampled <- mapM resample args
          msp resampled
          loopsV <- mapM gruu resampled
          let lengths = map SV.length loopsV
          massert $ all ((desiredLengthFrames * 2) ==) lengths

          let loop downKeys curPos = do
                newDownKeys <- processEvents downKeys
                --if newDownKeys /= downKeys then msp newDownKeys else return ()
                if newDownKeys /= downKeys then putStrLn (pressDiagram (length loopsV) newDownKeys) else return ()
                --msp ("nDK", newDownKeys)
                let buffer' = mixBuffers' loopsV curPos newDownKeys
                writeAudioAllAtOnce' buffer'

                let newCurPos = if curPos + theBufferSize >= desiredLengthFrames then 0 else curPos + theBufferSize
                --threadDelay 100000
                loop newDownKeys newCurPos

          loop downKeys 0

          term_audio
          putStrLn (show i)
