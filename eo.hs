import Control.Concurrent
import Control.Monad
import Data.Char (ord)
import Data.Heap
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import GHC.Stack
import Data.Ratio
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

data Pitch = Pitch Int
instance Show Pitch where
  show (Pitch p) = pitchToLetter p
data NoteOnOff = NoteOn | NoteOff
instance Show NoteOnOff where
  show NoteOn = "+"
  show NoteOff = "-"
data Event = Note NoteOnOff Pitch Int
instance Show Event where
  show (Note onOff pitch int) = (show onOff) ++ (show pitch)

data NoteAt = NoteAt Pitch 

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
sendmidiRenderEvent (Note onOff (Pitch pitch) vel) = intercalate " " ["channel", "1", (case onOff of NoteOn -> "note-on" ; NoteOff -> "note-off"), pitchToLetter pitch, show vel]

updateNoteSet :: Set.Set Event -> Event -> Set.Set Event
updateNoteSet noteSet (Note NoteOn pitch vel) =
  Set.insert (Note NoteOn pitch vel) noteSet
updateNoteSet noteSet (Note NoteOff pitch vel) =
  -- assert (Set.member (NoteOn pitch vel) noteSet)
  Set.delete (Note NoteOn pitch vel) noteSet
updateNoteSetMulti noteSet (e:es) = updateNoteSetMulti (updateNoteSet noteSet e) es
updateNoteSetMulti noteSet [] = noteSet

showNoteSet :: [Event] -> [String]
showNoteSet events = map show events

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

timeSignatureDenom = 4
bpm = 120

abstractTimeToSeconds at = (60.0 / bpm) * at
-- [(time, absnote)]
layers = [
  [(0, 0), (1, 1), (2, 2), (3, 3)],
  [(0.5, 2), (1.5, 3), (2.5, 0), (2.75, 0), (3.5, 1)] ]

ooo :: MinPrioHeap Double(Double, Double)
ooo = fromList [(1.3, (1.3, 3))]

combineLayers :: [[(Double, Double)]] -> MinPrioHeap Double (Int, Double, Double)
combineLayers layers = fromList $ concat $ map (\ix -> case ix of (layer, es) -> map (\e -> case e of (t, ni) -> (t, (layer, t, ni))) es) $ zip [0..] layers

playLayers = do
  let combined = combineLayers layers
   in do
     let loop :: Double -> MinPrioHeap Double (Int, Double, Double) -> IO ()
         loop currentTime events =  do
           readyEvents <- readReadyEvents
           case (view events) of
             Just ((t, event), rest) -> do
               sh $ show $ event
               if t > currentTime
                 then threadDelay $ round $ (t - currentTime) * 1000000
                 else return ()
               loop t rest
             Nothing -> do
               return ()
      in loop 0.0 combined

main = do
  sh "start"
  hSetBuffering stdout NoBuffering
  playLayers
