{-# LANGUAGE ScopedTypeVariables #-}
import Control.Concurrent (threadDelay)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as S
import Data.StorableVector.Base as SVB
import Foreign.C
import Foreign.Marshal.Array (mallocArray, copyArray)
--import Foreign.Marshal.Utils (fillBytes)
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable (peekElemOff, pokeElemOff, sizeOf)
import Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import System.Environment (getArgs)
import System.Exit
import System.IO

import Util

--foreign import ccall "fastestTextureVStrip" fastestTextureVStrip :: Ptr Word32 -> Ptr Word32 -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CDouble -> CDouble -> IO ()
--void fastestTextureVStrip(int32_t *start, int32_t *texPtr, int texWid, int texHt, int dPtr, int tx, int cy0, int cy1, double fty0, double dfty);

foreign import ccall "foo" foo :: CInt -> IO CInt
foreign import ccall "bar" bar :: Ptr CFloat -> IO ()

foreign import ccall "init_audio" init_audio :: IO ()
foreign import ccall "write_audio" write_audio :: Ptr Float -> Int -> IO ()
foreign import ccall "term_audio" term_audio :: IO ()

theBufferSize = 64
desiredLengthFrames = 44100 * 4

-- omg because I can't believe writing this is easier than finding one
omgResample :: Ptr Float -> Int -> Int -> IO (Ptr Float)
omgResample src srcNumFrames destNumFrames = do
  dest <- (mallocArray (destNumFrames * 2)) :: IO (Ptr Float)
  mapM_ (resamp src dest) [0..(srcNumFrames-1)]
  return dest
  where resamp :: Ptr Float -> Ptr Float -> Int -> IO ()
        resamp src dest srcI = let destI = floor $ (fromIntegral srcI) * ((fromIntegral destNumFrames) / (fromIntegral srcNumFrames))
                                in do sampleL <- peekElemOff src (toF (srcI * 2))
                                      pokeElemOff dest (toF (destI * 2)) sampleL
                                      sampleR <- peekElemOff src (toF (srcI * 2) + 1)
                                      pokeElemOff dest (toF (destI * 2) + 1) sampleR
        toF :: Int -> Int
        toF i = i -- * (sizeOf (undefined :: Float))

copyAndStereoize :: Int -> Int -> ForeignPtr Float -> IO (Ptr Float)
copyAndStereoize 2 numFrames fptr =
  -- Just copy
  do dest <- (mallocArray (numFrames * 2)) :: IO (Ptr Float)
     withForeignPtr fptr (\src -> copyArray src dest numFrames)
     return dest
copyAndStereoize 1 numFrames fptr =
  -- Duplicate each sample
  do dest <- (mallocArray (numFrames * 2)) :: IO (Ptr Float)
     withForeignPtr fptr $ dupSamples dest
     return dest
  where dupSamples dest src = do mapM_ (copy src dest) [0..(numFrames-1)]
        copy src dest offset = do sample <- peekElemOff src offset
                                  pokeElemOff dest (offset*2) sample
                                  pokeElemOff dest (offset*2 + 1) sample

gruu :: String -> IO (Ptr Float, Int)
gruu filename = do
  -- open the file that we want to know about
  --f <- SF.openFile "loop2.wav" SF.ReadMode SF.defaultInfo

  -- read the information about the file out
  --let info = SF.hInfo f

  --bruf :: BV.Buffer CFloat
  (info, Just (bruf :: BV.Buffer Float)) <- SF.readFile filename
  let v = BV.fromBuffer bruf
  let (fp, a, b) = SVB.toForeignPtr v
  msp (filename, fp, a, b)
  putStrLn $ assert (a==0) (show a)

  -- close the file
  --SF.hClose f

  -- display information about the file
  putStrLn $ "format:      " ++ (show $ SF.format info)
  putStrLn $ "sample rate: " ++ (show $ SF.samplerate info)
  putStrLn $ "channels:    " ++ (show $ SF.channels info)
  putStrLn $ "frames:      " ++ (show $ SF.frames info)

  --assertM "huhh" (a == 0) ()
  stereo <- copyAndStereoize (SF.channels info) (SF.frames info) fp
  return (stereo, b)

writeAudioAllAtOnce :: Int -> Ptr Float -> IO ()
writeAudioAllAtOnce bufferSize buffer = do write_audio buffer bufferSize

writeAudio :: Int -> Ptr Float -> IO ()
writeAudio bufferSize buffer = loop 0
  where loop sofar | sofar >= bufferSize = return ()
                   | otherwise = let remaining = bufferSize - sofar
                                     toWrite = min remaining theBufferSize
                                     size = (undefined :: Float) -- Haskell, you make-a me laugh
                                     subBufferStart = plusPtr buffer (sofar * (sizeOf size) * 2)
                                  in do write_audio subBufferStart toWrite
                                        loop (sofar + toWrite)

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

getActiveSamples :: [Ptr Float] -> S.Set Int -> [Ptr Float]
getActiveSamples loops active =
  map (\(_, loop) -> loop) $ filter (\(i, loop) -> isActive i) (zip [0..] loops)
  where isActive i = S.member (i + 36) active

-- mixBuffers resampled buffer newDownKeys
mixBuffers :: [Ptr Float] -> Ptr Float -> Int -> S.Set Int -> IO ()
mixBuffers loops buffer curPos downKeys =
  let remaining = desiredLengthFrames - curPos
      toWrite = min remaining theBufferSize
      --size = (undefined :: Float) -- Haskell, you make-a me laugh
      --subBufferStart = plusPtr buffer (sofar * (sizeOf size) * 2)
      activeLoops = getActiveSamples loops downKeys
   in do mapM_ (doIt activeLoops buffer) [curPos..(curPos+toWrite-1)]
         return ()
   where doIt :: [Ptr Float] -> Ptr Float -> Int -> IO ()
         doIt loops dest i = do 
                                --msp "hey"
                                --msp i
                                leftSamples <- mapM (getSample (i*2)) loops
                                rightSamples <- mapM (getSample (i*2+1)) loops
                                let leftSample = avg leftSamples
                                let rightSample = avg rightSamples
                                --msp leftSamples
                                --msp leftSample
                                let di = i - curPos
                                pokeElemOff dest (di*2) leftSample
                                pokeElemOff dest (di*2+1) rightSample
           where getSample fi src = do peekElemOff src fi
                 avg :: [Float] -> Float
                 avg [] = 0
                 avg samples = (sum samples) / fromIntegral (length samples)
   --in do write_audio subBufferStart toWrite
         --return $ sofar + toWrite

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
          --buffer :: Ptr CFloat

          loops <- mapM gruu args
          --(p, totalSize) <- gruu
          --msp ("yeahh", p, totalSize)
          --withForeignPtr fp (writeAudio totalSize)
          resampled <- mapM (\(p, totalSize) -> omgResample p totalSize desiredLengthFrames) loops
          --mapM (\(p, totalSize) -> writeAudio totalSize p) loops
          --mapM (\p -> writeAudio desiredLengthFrames p) resampled
          --writeAudioAllAtOnce totalSize p

          buffer <- (mallocArray (theBufferSize * 2)) :: IO (Ptr Float)

          let loop downKeys curPos = do
                newDownKeys <- processEvents downKeys
                if newDownKeys /= downKeys then msp newDownKeys else return ()
                --msp ("nDK", newDownKeys)
                mixBuffers resampled buffer curPos newDownKeys
                --msp ("writey", curPos)
                writeAudioAllAtOnce theBufferSize buffer
                let newCurPos = if curPos + theBufferSize >= desiredLengthFrames then 0 else curPos + theBufferSize
                --threadDelay 100000
                loop newDownKeys newCurPos

          loop downKeys 0

          --buffer <- (mallocArray theBufferSize) :: IO (Ptr CFloat)
          --pokeElemOff buffer 0 2.3
          --pokeElemOff buffer 1 4.5
          --bar buffer

          term_audio
          putStrLn (show i)
