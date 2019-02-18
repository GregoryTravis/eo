import Foreign.C
import Foreign.Marshal.Array (mallocArray)
--import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr
import Foreign.Storable (pokeElemOff)
import Sound.File.Sndfile as SF

--foreign import ccall "fastestTextureVStrip" fastestTextureVStrip :: Ptr Word32 -> Ptr Word32 -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CDouble -> CDouble -> IO ()
--void fastestTextureVStrip(int32_t *start, int32_t *texPtr, int texWid, int texHt, int dPtr, int tx, int cy0, int cy1, double fty0, double dfty);

foreign import ccall "foo" foo :: CInt -> IO CInt
foreign import ccall "bar" bar :: Ptr CFloat -> IO ()

foreign import ccall "init_audio" init_audio :: IO ()
foreign import ccall "term_audio" term_audio :: IO ()

bufferSize = 64



gruu :: IO ()
gruu = do
  -- open the file that we want to know about
  f <- SF.openFile "loop2.wav" SF.ReadMode SF.defaultInfo

  -- read the information about the file out
  let info = SF.hInfo f

  -- close the file
  SF.hClose f

  -- display information about the file
  putStrLn $ "format:      " ++ (show $ SF.format info)
  putStrLn $ "sample rate: " ++ (show $ SF.samplerate info)
  putStrLn $ "channels:    " ++ (show $ SF.channels info)
  putStrLn $ "frames:      " ++ (show $ SF.frames info)


main = do putStrLn "asdf"
          gruu
          i <- foo 12
          putStrLn "asdf2"
          init_audio
          putStrLn (show i)
          --buffer :: Ptr CFloat
          buffer <- (mallocArray bufferSize) :: IO (Ptr CFloat)
          pokeElemOff buffer 0 2.3
          pokeElemOff buffer 1 4.5
          bar buffer
          term_audio
          putStrLn (show i)
