import Control.Concurrent
import Control.Concurrent
import Control.Monad
import Data.List
import GHC.Stack
import System.IO

assert b v = if b
  then v
  else error "um"

sh x = do hPutStrLn stderr x

isNote x = isInfixOf "note" x

logm x = if isNote x
           then sh x
           else return ()

modes = [[0, 4, 7], [0, 3, 7, 10]]
--0isCtrl x = x == 48 || x == 49

data Note = Note Int Int deriving Show

parseNote line =
  case (words line) of
    [channel, one, eventType, pitchS, velocityS] ->
      assert (channel == "channel")
        assert (one == "1")
          assert (eventType == "note-on" || eventType == "note-off")
            Note 3 4

guh = do
  forever $ do
    let
      loop = do
        ready <- hReady stdin
        sh $ "ready" ++ (show ready)
        if ready
          then do
            line <- getLine
            sh $ "yah " ++ line
            if isNote line
              then do
                logm line
                putStrLn line
                sh $ show $ words line
                sh $ show $ parseNote line
                -- stack <- currentCallStack
                -- sh $ show stack
                -- sh (show (words line))
                -- putStrLn "note-on 60 127"
              else do
                return ()
            loop
          else do
            return ()
      in loop
    threadDelay 1000000

main = do
  sh "start"
  hSetBuffering stdout NoBuffering
  -- geee 40
  guh
