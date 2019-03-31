{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

--import Data.Fixed (mod')
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
class DStream a e where
  next :: a -> Time -> (e, a)

data Cyc = Cyc Int [Int]
  deriving Show

instance DStream Cyc (Event Int) where
  next c@(Cyc beatDur values) now = (Event nextValue now, c)
    where nextValue = values !! (mod ((now `div` beatDur) + 1) (length values))

main = do
  let cs = Cyc 500 [10, 11, 12, 13]
  msp $ (next cs 250 :: (Event Int, Cyc))
  msp $ (next cs 750 :: (Event Int, Cyc))
  msp "hi"
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
