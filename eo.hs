import Control.Concurrent
import Control.Monad
import Data.Char (ord)
import Data.Heap
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
--import Debug.Trace
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
  --show (Pitch p) = show p
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
offsetNotes = Map.fromList $ map (\pr -> case pr of (c, o) -> (o, c)) (Map.toList noteOffsets)
whiteKeys = [0, 2, 4, 5, 7, 9, 11]
pitchToWhiteKey p = fromJust $ find (p ==) whiteKeys

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

type NoteSet = Set.Set Event

updateNoteSet :: NoteSet -> Event -> NoteSet
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
      -- sh $ "ready" ++ (show ready)
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
data AbstractNote = AbstractNote Double Int deriving Show
data AbstractSequence = AbstractSequence [[AbstractNote]]
listToAbstractSequence :: [[(Double, Int)]] -> AbstractSequence
listToAbstractSequence pss =
  AbstractSequence $ map (\ps -> map (\p -> case p of (t, p) -> AbstractNote t p) ps) pss
aSequence = listToAbstractSequence [
  [(0, 0), (1, 1), (2, 2), (3, 3)],
  [(0.5, 2), (1.5, 3), (2.5, 0), (2.75, 0), (3.5, 1)] ]

-- ooo :: MinPrioHeap Double(Double, Double)
-- ooo = fromList [(1.3, (1.3, 3))]

combineLayers :: AbstractSequence -> MinPrioHeap Double (Int, AbstractNote)
combineLayers (AbstractSequence layers) = fromList $ concat $
  map groo $ zip [0..] layers
  where boo layer n@(AbstractNote t ni) = (t, (layer, n))
        groo (layer, es) = map (boo layer) es

class Show a => Controller a where
  --empty :: a
  processEvent :: a -> Event -> a

processEvents :: Controller i => i -> [Event] -> i
processEvents inst (e:es) = processEvents (processEvent inst e) es
processEvents inst [] = inst

class (Show a, Controller a) => Inst a where
  --processEvent :: a -> Event -> a
  processAbstractNote :: absNote -> a -> ()

data LayerControl = LayerControl (Int, Int) (Set.Set Int) deriving Show
instance Controller LayerControl where
  --empty = LayerControl Set.empty
  processEvent x@(LayerControl range layerSet) (Note onOff (Pitch p) _) = if (inRange p range) then LayerControl range ((case onOff of NoteOn -> Set.insert ; NoteOff -> Set.delete) (pitchToLayer p range) layerSet) else x
    where pitchToLayer p (lo, hi) = pitchToWhiteKey (p - lo)
          inRange p (lo, hi) = p >= lo && p <= hi

data TheInst = TheInst LayerControl deriving Show
instance Controller TheInst where
  processEvent (TheInst controller) event = TheInst (processEvent controller event)
instance Inst TheInst where
  processAbstractNote abstractNote inst = ()

playLayers = do
  let combined = combineLayers aSequence
   in do
     let loop :: Inst i => i -> Double -> MinPrioHeap Double (Int, AbstractNote) -> IO ()
         loop inst currentTime events =  do
           case (view events) of
             Just ((t, (layer, absNote)), rest) -> do
               if t > currentTime
                 then threadDelay $ round $ (t - currentTime) * 1000000
                 else return ()
{-
               let isOn = isLayerOn inst event
                   plusMinus = if isOn then "+" else "-"
               sh $ plusMinus ++ " " ++ (show event)
-}
               sh $ (show absNote) ++ " " ++ (show layer)
               readyEvents <- readReadyEvents
               let updatedInst = processEvents inst readyEvents
               sh $ show updatedInst
               loop updatedInst t rest
             Nothing -> do
               return ()
      in loop (TheInst (LayerControl (48, 48+11) Set.empty)) 0.0 combined

main = do
  sh "start"
  hSetBuffering stdout NoBuffering
  playLayers
