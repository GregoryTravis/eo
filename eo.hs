import Control.Concurrent
import Control.Concurrent
import Control.Monad
import Data.Char (ord)
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe
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
data NoteOnOff = NoteOn | NoteOff
instance Show NoteOnOff where
  show NoteOn = "+"
  show NoteOff = "-"
data Event = Note NoteOnOff Pitch Int
  deriving Show

instance Eq Pitch where
  (Pitch a) == (Pitch b) = a == b
instance Eq Event where
  (Note _ pitchA _) == (Note _ pitchB _) = pitchA == pitchB
instance Ord Pitch where
  compare (Pitch a) (Pitch b) = compare a b
instance Ord Event where
  compare (Note _ pitchA velA) (Note _ pitchB velB) =
    compare (pitchA, velA) (pitchB, velB)

middleCNum = 4 -- Is this right?

atoi s = read s :: Int

noteOffsets = Map.fromList [('A', 9), ('B', 11), ('C', 0), ('D', 2), ('E', 4), ('F', 5), ('G', 7)]

-- parsePitch :: [Char] -> Int
parsePitch [letter, octave] = ((atoi [octave] + 1) * 12) + fromJust (Map.lookup letter noteOffsets) -- ((ord letter) - (ord 'A') - 2)
parsePitch [letter, sharp, octave] = parsePitch [letter, octave] + 1

parseEvent line =
  case (words line) of
    [channel, one, eventType, pitchS, velocityS] ->
      assert (channel == "channel")
        assert (one == "1")
          assert (eventType == "note-on" || eventType == "note-off")
            Note (if eventType == "note-on" then NoteOn else NoteOff) (Pitch (parsePitch pitchS)) (read velocityS :: Int)

pitchToLetter num = case divMod num 12 of
  (d, m) -> (theOctave !! m) ++ (show (d - 1))
renderEvent (Note onOff (Pitch pitch) vel) = intercalate " " ["channel", "1", (case onOff of NoteOn -> "note-on" ; NoteOff -> "note-off"), pitchToLetter pitch, show vel]

updateNoteSet :: Set.Set Event -> Event -> Set.Set Event
updateNoteSet noteSet (Note NoteOn pitch vel) =
  Set.insert (Note NoteOn pitch vel) noteSet
updateNoteSet noteSet (Note NoteOff pitch vel) =
  -- assert (Set.member (NoteOn pitch vel) noteSet)
  Set.delete (Note NoteOn pitch vel) noteSet
updateNoteSetMulti noteSet (e:es) = updateNoteSetMulti (updateNoteSet noteSet e) es
updateNoteSetMulti noteSet [] = noteSet

briefShow (Note onOff (Pitch pitch) vel) = pitchToLetter pitch
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

-- todo: use where; remove spaces around :, remove parens
splitBy events pred =
  let foo (yes, no) (e : es) = if (pred e)
                                 then (foo (e : yes, no) es)
                                 else (foo (yes, e : no) es)
      foo results [] = results
   in foo ([], []) events

processEvents :: (Set.Set Event, Set.Set Event) -> [Event] -> (Set.Set Event, Set.Set Event)
-- todo indent and remove parens
processEvents (chord, noteSet) events = -- updateNoteSetMulti noteSet events
  let (control, performance) = splitBy events (\e -> case e of (Note NoteOn (Pitch pitch) vel) -> (pitch >= 48 && pitch < 60))
   in (updateNoteSetMulti chord control, updateNoteSetMulti noteSet performance)

guh =
  let loop chord noteSet = do
        events <- readReadyEvents
        sh $ show events
        sh $ show $ showNoteSet events
        threadDelay 1000000
        let (updatedChord, updatedNoteSet) = processEvents (chord, noteSet) events
         in do sh $ "perf " ++ (show $ showNoteSet $ Set.toList updatedNoteSet)
               sh $ "ctrl " ++ (show $ showNoteSet $ Set.toList updatedChord)
               loop updatedChord updatedNoteSet
   in loop Set.empty Set.empty

main = do
  sh "start"
  hSetBuffering stdout NoBuffering
  -- geee 40
  guh
