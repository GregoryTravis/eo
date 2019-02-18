{-# LANGUAGE ScopedTypeVariables #-}
import Data.StorableVector.Base as SVB
import Foreign.C
import Foreign.Marshal.Array (mallocArray, copyArray)
--import Foreign.Marshal.Utils (fillBytes)
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable (peekElemOff, pokeElemOff, sizeOf)
import Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.StorableVector as BV

import Util

--foreign import ccall "fastestTextureVStrip" fastestTextureVStrip :: Ptr Word32 -> Ptr Word32 -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CDouble -> CDouble -> IO ()
--void fastestTextureVStrip(int32_t *start, int32_t *texPtr, int texWid, int texHt, int dPtr, int tx, int cy0, int cy1, double fty0, double dfty);

foreign import ccall "foo" foo :: CInt -> IO CInt
foreign import ccall "bar" bar :: Ptr CFloat -> IO ()

foreign import ccall "init_audio" init_audio :: IO ()
foreign import ccall "write_audio" write_audio :: Ptr Float -> Int -> IO ()
foreign import ccall "term_audio" term_audio :: IO ()

theBufferSize = 64

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

gruu :: IO (Ptr Float, Int)
gruu = do
  -- open the file that we want to know about
  --f <- SF.openFile "loop2.wav" SF.ReadMode SF.defaultInfo

  -- read the information about the file out
  --let info = SF.hInfo f

  --bruf :: BV.Buffer CFloat
  (info, Just (bruf :: BV.Buffer Float)) <- SF.readFile "loop2.wav"
  let v = BV.fromBuffer bruf
  let (fp, a, b) = SVB.toForeignPtr v
  msp (fp, a, b)
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


main = do putStrLn "asdf"
          i <- foo 12
          putStrLn "asdf2"
          init_audio
          putStrLn (show i)
          --buffer :: Ptr CFloat

          (p, totalSize) <- gruu
          msp ("yeahh", p, totalSize)
          --withForeignPtr fp (writeAudio totalSize)
          writeAudio totalSize p
          --writeAudioAllAtOnce totalSize p

          buffer <- (mallocArray theBufferSize) :: IO (Ptr CFloat)
          pokeElemOff buffer 0 2.3
          pokeElemOff buffer 1 4.5
          bar buffer

          term_audio
          putStrLn (show i)
