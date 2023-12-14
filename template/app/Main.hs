module Main where

import Data.Char qualified as Char
import Data.Function ((&))
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Maybe qualified as M
import Data.Ord qualified as O
import Data.Set qualified as S
import Debug.Trace (trace)
import System.IO ()

main :: IO ()
main = do
  print "here!"
  print "but not here!"
  inp <- readFile testInput
  print inp

testInput :: String
testInput = "test.input"
realInput :: String
realInput = "1.input"
