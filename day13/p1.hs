#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , array
             , split
             , text
             , containers
default-extensions:
  ImportQualifiedPost
-}

import Data.Array qualified as A
import Data.Char qualified as Char
import Data.Function ((&))
import Data.List qualified as L
import Data.List.Split (splitOn)
import Data.Maybe qualified as Maybe
import Data.Ord qualified as O
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace (trace)

import System.IO

-- 0 1 2 3 4 5 6 7 8
-- # . # # . . # # . | 0
-- . . # . # # . # . | 1
-- # # . . . . . . # | 2
-- # # . . . . . . # | 3
-- . . # . # # . # . | 4
-- . . # # . . # # . | 5
-- # . # . # # . # . | 6

type Grid = A.Array (Int, Int) Char

mirrorToArray :: String -> Grid
mirrorToArray mirror =
  A.array ((0, 0), (rows - 1, cols - 1)) mirrorMap
 where
  gridLines = lines mirror
  rows = length gridLines
  cols = length (head gridLines)
  mirrorMap =
    zip [0 ..] gridLines
      & concatMap (\(i, line) -> zipWith (\j c -> ((i, j), c)) [0 ..] line)

findHorizontal :: Grid -> Maybe Int
findHorizontal grid =
  Maybe.listToMaybe [i | i <- [1 .. rows], isSymmetricHorizontal i grid]
 where
  (_, (rows, cols)) = A.bounds grid
  isSymmetricHorizontal i grid = all (\d -> rowsSame (i - d - 1) (i + d)) [0 .. w - 1]
   where
    w = min i (rows - i + 1)
    rowsSame l m = all (\j -> grid A.! (l, j) == grid A.! (m, j)) [0 .. cols]

findHorizontal' :: Grid -> Int -> Maybe Int
findHorizontal' grid delta =
  Maybe.listToMaybe [i | i <- [1 .. rows], isSymmetricHorizontal' i]
 where
  (_, (rows, cols)) = A.bounds grid
  isSymmetricHorizontal' i = delta == (sum $ map (\d -> rowsSame (i - d - 1) (i + d)) [0 .. w - 1])
   where
    w = min i (rows - i + 1)
    rowsSame l m = sum $ map (\j -> if grid A.! (l, j) == grid A.! (m, j) then 0 else 1) [0 .. cols]

parse :: String -> [(Grid, Grid)]
parse input =
  map (\(x, y) -> (mirrorToArray x, mirrorToArray y)) pairs
 where
  mirrors = splitOn "\n\n" input
  pairs = map (\x -> (x, lines x & L.transpose & unlines)) mirrors

p1 :: (Grid, Grid) -> Int
p1 (og, ng) = Maybe.fromMaybe 0 (findHorizontal ng) + 100 * Maybe.fromMaybe 0 (findHorizontal og)

p2 :: (Grid, Grid) -> Int
p2 (og, ng) = Maybe.fromMaybe 0 (findHorizontal' ng 1) + 100 * Maybe.fromMaybe 0 (findHorizontal' og 1)

main :: IO ()
main = do
  input <- readFile "1.input"
  let grids = parse input
  print (sum $ map p1 grids)
  print (sum $ map p2 grids)
