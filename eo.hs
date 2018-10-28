import Control.Concurrent
import Control.Concurrent
import Control.Monad
import Data.Char (ord)
import Data.List
import Data.List.Split
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

theOctave = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

data Pitch = Pitch Int deriving Show
data Event = NoteOn Pitch Int | NoteOff Pitch Int
  deriving Show

middleCNum = 4 -- Is this right?

atoi s = read s :: Int

-- parsePitch :: [Char] -> Int
parsePitch [letter, octave] = ((atoi [octave] + 1) * 12) + ((ord letter) - (ord 'A') - 2)
parsePitch [letter, sharp, octave] = parsePitch [letter, octave] + 1

parseEvent line =
  case (words line) of
    [channel, one, eventType, pitchS, velocityS] ->
      assert (channel == "channel")
        assert (one == "1")
          assert (eventType == "note-on" || eventType == "note-off")
            (if eventType == "note-on" then NoteOn else NoteOff) (Pitch (parsePitch pitchS)) (read velocityS :: Int)

pitchToLetter num = case divMod num 12 of
  (d, m) -> (theOctave !! m) ++ (show (d - 1))
renderEvent (NoteOn (Pitch pitch) vel) = intercalate " " ["channel", "1", "note-on", pitchToLetter pitch, show vel]
renderEvent (NoteOff (Pitch pitch) vel) = intercalate " " ["channel", "1", "note-off", pitchToLetter pitch, show vel]

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
                -- sh $ show $ words line
                sh $ show $ parseEvent line
                sh $ show $ renderEvent (parseEvent line)
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
