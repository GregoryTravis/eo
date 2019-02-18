import Foreign.C
--import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr
--import Foreign.Storable (pokeElemOff)

--foreign import ccall "fastestTextureVStrip" fastestTextureVStrip :: Ptr Word32 -> Ptr Word32 -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CDouble -> CDouble -> IO ()
--void fastestTextureVStrip(int32_t *start, int32_t *texPtr, int texWid, int texHt, int dPtr, int tx, int cy0, int cy1, double fty0, double dfty);

foreign import ccall "foo" foo :: CInt -> IO CInt

foreign import ccall "init_audio" init_audio :: IO ()
foreign import ccall "term_audio" term_audio :: IO ()

main = do putStrLn "asdf"
          i <- foo 12
          putStrLn "asdf2"
          init_audio
          putStrLn (show i)
          term_audio
          putStrLn (show i)
