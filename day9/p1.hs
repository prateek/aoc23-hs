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

-- 0 3 6 9 12 15
-- 1 3 6 10 15 21
-- 10 13 16 21 30 45

parseInput :: String -> [[Int]]
parseInput input =
  map readLine $ lines input
 where
  readInt x = read x :: Int
  readLine l = map readInt $ words l

p1 input =
  map go parsed
 where
  parsed = parseInput input
  go x = successiveDiffs x & flipped & extrapolate

extrapolate :: [[Int]] -> Int
extrapolate =
  foldl (\acc x -> acc + head x) 0

flipped :: [[Int]] -> [[Int]]
flipped l = reverse $ map reverse l

successiveDiffs :: [Int] -> [[Int]]
successiveDiffs l
  | all (== 0) l = [l]
  | otherwise = l : successiveDiffs (diff l)

diff :: [Int] -> [Int]
diff [] = []
diff [_] = []
diff (x : y : xs) = (y - x) : diff (y : xs)

main :: IO ()
main = do
  input <- readFile "1.input"
  print (sum $ p1 input)
