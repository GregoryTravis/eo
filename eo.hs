import Control.Concurrent
import Control.Monad
import Data.List
import System.IO

sh x = do hPutStrLn stderr x

isNote x = isInfixOf "note" x

logm x = if isNote x
           then sh x
           else return ()

modes = [[0, 4, 7], [0, 3, 7, 10]]
--0isCtrl x = x == 48 || x == 49

guh = do
  forever $ do
    line <- getLine                                     -- line :: String
    if isNote line
      then do
        logm line
        putStrLn line
        -- putStrLn "note-on 60 127"
      else return ()

main = do
  hSetBuffering stdout NoBuffering
  -- geee 40
  guh
