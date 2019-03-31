module Main where

import System.IO

import Util

main = do
  b <- hWaitForInput stdin 1000
  msp b
  if b
    then do line <- hGetLine stdin
            msp ("line", line)
    else return ()
