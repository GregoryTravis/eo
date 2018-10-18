import Control.Concurrent
import Control.Monad
import Data.List
import System.IO

sh x = do hPutStrLn stderr x

logm x = if isInfixOf "note" x
           then sh x
           else return ()

guh = do
  forever $ do
    line <- getLine                                     -- line :: String
    logm line
    putStrLn line

main = do
  hSetBuffering stdout NoBuffering
  -- geee 40
  guh
