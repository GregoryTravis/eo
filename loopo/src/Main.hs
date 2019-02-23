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
import Foreign.Storable (peekElemOff, pokeElemOff, sizeOf, Storable)
import Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Process

import Util

--foreign import ccall "fastestTextureVStrip" fastestTextureVStrip :: Ptr Word32 -> Ptr Word32 -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CDouble -> CDouble -> IO ()
--void fastestTextureVStrip(int32_t *start, int32_t *texPtr, int texWid, int texHt, int dPtr, int tx, int cy0, int cy1, double fty0, double dfty);

foreign import ccall "foo" foo :: CInt -> IO CInt
foreign import ccall "bar" bar :: Ptr CFloat -> IO ()

foreign import ccall "init_audio" init_audio :: IO ()
foreign import ccall "write_audio" write_audio :: Ptr Float -> Int -> IO ()
foreign import ccall "term_audio" term_audio :: IO ()

theBufferSize = 64
desiredLengthFrames = 175683 -- 44100 * 2
lowKey = 36

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

-- Concatenate enough copies of the vector to reach the standard length; must
-- be an integral number of copies.
maybeLoop :: Storable a => SV.Vector a -> SV.Vector a
maybeLoop v =
  let (numCopies, 0) = divMod desiredLengthFrames (SV.length v)
   in SV.concat (replicate numCopies v)

gruu :: String -> IO (Ptr Float, Int)
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

ptrToVector length ptr = do
  fp <- newForeignPtr_ ptr
  return $ SVB.fromForeignPtr fp length

writeAudioAllAtOnce :: Int -> Ptr Float -> IO ()
writeAudioAllAtOnce bufferSize buffer = do write_audio buffer bufferSize

writeAudioAllAtOnce' :: Vector Float -> IO ()
writeAudioAllAtOnce' v =
  --let (fp, start, length) = SVB.toForeignPtr v
      --aFloat = (undefined :: Float)
   --in withForeignPtr fp (\ptr -> writeAudioAllAtOnce length (plusPtr ptr (start * 2 * (sizeOf aFloat))))
  let (fp, 0, length) = SVB.toForeignPtr v
   in withForeignPtr fp (\ptr -> writeAudioAllAtOnce (length `div` 2) ptr)

writeAudioAllAtOnce'' :: Int -> Ptr Float -> IO ()
writeAudioAllAtOnce'' length ptr = do
  v <- ptrToVector length ptr
  writeAudioAllAtOnce' v
  --fp <- newForeignPtr_ ptr
  --let v = SVB.fromForeignPtr fp length
   --in writeAudioAllAtOnce' v

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

getActiveSamples :: [a] -> S.Set Int -> [a]
getActiveSamples loops active =
  map (\(_, loop) -> loop) $ filter (\(i, loop) -> isActive i) (zip [0..] loops)
  where isActive i = S.member (i + lowKey) active

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
                                let leftSample = sum leftSamples
                                let rightSample = sum rightSamples
                                --msp leftSamples
                                --msp leftSample
                                let di = i - curPos
                                pokeElemOff dest (di*2) leftSample
                                pokeElemOff dest (di*2+1) rightSample
           where getSample fi src = do peekElemOff src fi
                 avg :: [Float] -> Float
                 avg [] = 0
                 avg samples = (sum samples) / fromIntegral (length samples)

mixBuffers' :: [Vector Float] -> Int -> S.Set Int -> Vector Float
mixBuffers' loops curPos downKeys =
  let remaining = desiredLengthFrames - curPos
      toWrite = min remaining theBufferSize
      --size = (undefined :: Float) -- Haskell, you make-a me laugh
      --subBufferStart = plusPtr buffer (sofar * (sizeOf size) * 2)
      activeLoops = getActiveSamples loops downKeys
      --activeSubLoops = map (\loop -> take toWrite (drop curPos loop)) activeLoops
      --activeLoopsLists = map SV.unpack activeLoops
      --sublists = map (\loop -> take toWrite (drop curPos loop))
   in SV.sample (toWrite * 2) (mixSamples activeLoops)
   --in SV.take toWrite (SV.drop curPos (loops !! 0))
  where mixSamples :: [Vector Float] -> Int -> Float
        --mixSamples loops i = sum $ map (\loop -> (SV.index loop (eesp (show (curPos, i, ((curPos * 2) + i))) ((curPos * 2) + i)))) loops
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
          --buffer :: Ptr CFloat

          resampled <- mapM resample args
          msp resampled
          --(loops, lengths) <- unzip $ mapM gruu resampled
          x0 <- mapM gruu resampled :: IO [(Ptr Float, Int)]
          let x1 = unzip x0 :: ([Ptr Float], [Int])
          let (loops, lengths) = x1
          msp "whu"
          msp lengths
          msp $ map (desiredLengthFrames ==) lengths
          msp $ all (desiredLengthFrames ==) lengths
          massert $ all (desiredLengthFrames ==) lengths
          loopsV <- mapM (ptrToVector (desiredLengthFrames * 2)) loops
          --(p, totalSize) <- gruu
          --msp ("yeahh", p, totalSize)
          --withForeignPtr fp (writeAudio totalSize)
          --resampled <- mapM (\(p, totalSize) -> omgResample p totalSize desiredLengthFrames) loops
          --mapM (\(p, totalSize) -> writeAudio totalSize p) loops
          --mapM (\p -> writeAudio desiredLengthFrames p) resampled
          --writeAudioAllAtOnce totalSize p

          buffer <- (mallocArray (theBufferSize * 2)) :: IO (Ptr Float)

          let loop downKeys curPos = do
                newDownKeys <- processEvents downKeys
                --if newDownKeys /= downKeys then msp newDownKeys else return ()
                if newDownKeys /= downKeys then putStrLn (pressDiagram (length loops) newDownKeys) else return ()
                --msp ("nDK", newDownKeys)
                --mixBuffers loops buffer curPos newDownKeys
                --time "old" $ mixBuffers loops buffer curPos newDownKeys
                buffer' <- (time "new" $ return $ mixBuffers' loopsV curPos newDownKeys) :: IO (Vector Float)
                --let buffer' = mixBuffers' loopsV curPos newDownKeys
                --msp ("writey", curPos)

                --bufpo <- (ptrToVector (theBufferSize * 2) buffer) :: IO (Vector Float)
                --msp ("glee", SV.length buffer', SV.length bufpo, bufpo == buffer')

                --msp ("wheesh", bufpo == buffer')
                --msp ("old", SV.unpack bufpo)
                --msp ("new", SV.unpack buffer')

                --writeAudioAllAtOnce theBufferSize buffer
                --writeAudioAllAtOnce'' theBufferSize buffer
                writeAudioAllAtOnce' buffer'
                --writeAudioAllAtOnce' bufpo

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
