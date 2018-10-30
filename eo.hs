import Control.Concurrent
import Control.Concurrent
import Control.Monad
import Data.Char (ord)
import Data.List
import Data.List.Split
import qualified Data.Set as Set
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
eventPitch (NoteOn pitch vel) = pitch
eventPitch (NoteOff pitch vel) = pitch
eventVel (NoteOn pitch vel) = vel
eventVel (NoteOff pitch vel) = vel

instance Eq Pitch where
  (Pitch a) == (Pitch b) = a == b
instance Eq Event where
  e0 == e1 = (eventPitch e0) == (eventPitch e1) && (eventVel e0) == (eventVel e1)
instance Ord Pitch where
  compare (Pitch a) (Pitch b) = compare a b
instance Ord Event where
  compare e0 e1 = if (eventPitch e0) == (eventPitch e1)
    then compare (eventVel e0) (eventVel e1)
    else compare (eventPitch e0) (eventPitch e1)

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

updateNoteSet :: Set.Set Event -> Event -> Set.Set Event
updateNoteSet noteSet (NoteOn pitch vel) =
  Set.insert (NoteOn pitch vel) noteSet
updateNoteSet noteSet (NoteOff pitch vel) =
  -- assert (Set.member (NoteOn pitch vel) noteSet)
  Set.delete (NoteOn pitch vel) noteSet
updateNoteSetMulti noteSet (e:es) = updateNoteSetMulti (updateNoteSet noteSet e) es
updateNoteSetMulti noteSet [] = noteSet

briefShow (NoteOn (Pitch pitch) vel) = pitchToLetter pitch
briefShow (NoteOff (Pitch pitch) vel) = pitchToLetter pitch
showNoteSet :: [Event] -> [String]
showNoteSet events = map briefShow events

processEventLine :: String -> Maybe Event
processEventLine line =
  if isNote line
    then Just $ parseEvent line
    else Nothing

readReadyEvents :: IO [Event]
readReadyEvents =
  let loop events = do
      ready <- hReady stdin
      sh $ "ready" ++ (show ready)
      if ready
        then do
          line <- getLine
          sh $ "yah " ++ line
          if isNote line then case (processEventLine line) of
            Just event -> loop $ events ++ [event]
            Nothing -> loop events
            else loop events
        else return events
   in loop []

guh =
  let loop noteSet = do
        events <- readReadyEvents
        sh $ show $ showNoteSet events
        threadDelay 1000000
        let updatedNoteSet = (updateNoteSetMulti noteSet events)
         in do sh $ show $ showNoteSet $ Set.toList updatedNoteSet
               loop updatedNoteSet
   in loop Set.empty

main = do
  sh "start"
  hSetBuffering stdout NoBuffering
  -- geee 40
  guh
