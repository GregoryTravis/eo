{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

--import Data.Fixed (mod')
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO

import Util

-- Milliseconds
type Time = Int
type Duration = Int

data Event a = Event a Time
  deriving Show

{-
class Event a where
  getTime :: a -> Time
  payload :: a -> a
-}

-- Deterministic stream: returns an event and the continuation stream
class DStream a e | a -> e where
  next :: a -> Time -> (Event e, a)

data Cyc = Cyc Int [Int]
  deriving Show

instance DStream Cyc Int where
  next c@(Cyc beatDur values) now = (Event nextValue eTime, c)
    where nextValue = values !! (mod ((now `div` beatDur) + 1) (length values))
          eTime = (((now `div` beatDur) + 1) * beatDur)

class NStream a e | a -> e where
  nnext :: a -> Time -> Duration -> IO (Maybe (Event e, a))

data OnOff = On | Off
  deriving Show
data Midi = Midi Int OnOff
  deriving Show

parse :: String -> Midi
parse line = Midi 60 On

data MidiStdin = MidiStdin Time
  deriving Show

instance NStream MidiStdin Midi where
  nnext m@(MidiStdin startTime) _ timeout =
    do b <- hWaitForInput stdin timeout
       now <- getPOSIXTime
       --msp b
       if b
         then do line <- hGetLine stdin
                 --msp ("line", line)
                 msp ("huh", now, startTime)
                 return $ Just (Event (parse line) (truncate now - startTime), m)
         else return Nothing

--data NDStream n ne d de = NDStream (NStream n ne) (DStream d de)
data NDStream n d = NDStream n d
  deriving Show

--instance (NStream n ne, DStream d de) => NStream (NDStream n ne d de) (Event (Either ne de)) where
instance (Show n, Show d, Show ne, Show de, NStream n ne, DStream d de) => NStream (NDStream n d) (Either ne de) where
  nnext (NDStream n d) now timeout =
    do let q@((Event dEvent dTime), nextDStream) = next d now
       msp ("q", q)
       x <- nnext n now (dTime - now)
       msp ("x", x, now)
       case x of Nothing -> return $ Just (Event (Right dEvent) dTime, NDStream n nextDStream)
                 Just (Event nEvent nTime, nextNStream) -> return $ Just (Event (Left nEvent) nTime, NDStream nextNStream d)

main = do
  let cs = Cyc 2000 [10, 11, 12, 13]
  --msp $ next cs 250
  --msp $ next cs 750
  --msp "hi"
  now <- getPOSIXTime
  let ms = MidiStdin (truncate now)
  let bs = NDStream ms cs
  e <- nnext bs 100 10000
  msp e
  --e2 <- nnext ms (truncate now + 100) 600
  --msp e2
  return ()

_main = do
  b <- hWaitForInput stdin 1000
  msp b
  if b
    then do line <- hGetLine stdin
            msp ("line", line)
    else return ()

{-
-- Nondeterministic stream: requires a timeout
class NStream a where
  next :: NStream a -> Time -> Duration -> IO (Maybe (Event a, NStream a))

data NDStream n d = NDStream (NStream n) (DStream d)

instance (NStream n, DStream d) => NStream (NDStream n d)
  -- next :: NStream (Either n d) -> Time -> Duration -> IO (Maybe (Event (Either n d), NStream (Either n d)))
  next (NDStream n d) now =
    let (dEvent, nextDStream) = next d now
        dEventTime = gtTime dEvent
     in do nNext <- next n now (dEventTime - now)
           return case nNext of Nothing -> Just (Left dEvent, NDStream n nextDStream)
                                Just (Right nEvent, nextNStream) -> Just (nEvent
-}

{-
data Foo = Foo Int

data Bar = Bar Int Int

instance Event Bar where
  getTime (Bar a b) = 7
  payload (Bar a b) = a

instance DStream Foo (Int, Int) where
  next (Foo i) now = ((3, 4), (Foo (i+1)))
-}
