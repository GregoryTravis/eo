import Foreign.C
import Foreign.Marshal.Array (mallocArray)
--import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr
import Foreign.Storable (pokeElemOff)

--foreign import ccall "fastestTextureVStrip" fastestTextureVStrip :: Ptr Word32 -> Ptr Word32 -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CDouble -> CDouble -> IO ()
--void fastestTextureVStrip(int32_t *start, int32_t *texPtr, int texWid, int texHt, int dPtr, int tx, int cy0, int cy1, double fty0, double dfty);

foreign import ccall "foo" foo :: CInt -> IO CInt
foreign import ccall "bar" bar :: Ptr CFloat -> IO ()

foreign import ccall "init_audio" init_audio :: IO ()
foreign import ccall "term_audio" term_audio :: IO ()

bufferSize = 64

main = do putStrLn "asdf"
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
