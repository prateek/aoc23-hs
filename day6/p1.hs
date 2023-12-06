#!/usr/bin/env runhaskell

import Data.Char qualified as Char
import Data.Function ((&))
import Data.List qualified as L
import Data.Maybe qualified as M
import Data.Ord qualified as O
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace (trace)

import System.IO

testInput :: [(Int, Int)]
testInput = [(7, 9), (15, 40), (30, 200)]

testInput' :: [(Int, Int)]
testInput' = [(71530, 940200)]

realInput :: [(Int, Int)]
realInput = [(63, 411), (78, 1274), (94, 2047), (68, 1035)]
realInput' :: [(Int, Int)]
realInput' = [(63789468, 411127420471035)]

-- input: (maxTime, distanceToBeat)

-- input: maxTime
-- output: [(holdTime, travelTime)]
genAllHoldTimes :: Int -> [(Int, Int)]
genAllHoldTimes t = [(x, t - x) | x <- [0 .. t]]

distance :: (Int, Int) -> Int
distance (holdTime, travelTime) = holdTime * travelTime

numWaysToBeat (maxTime, distanceToBeat) =
  genAllHoldTimes maxTime
    & filter (\(holdTime, travelTime) -> distance (holdTime, travelTime) > distanceToBeat)
    & length

p1 = product . map numWaysToBeat

main :: IO ()
main = do
  print (p1 realInput)
  print (numWaysToBeat (head testInput'))
  print (numWaysToBeat (head realInput'))
