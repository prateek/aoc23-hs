#!/usr/bin/env runhaskell

import Data.Char qualified as Char
import Data.Function ((&))
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Maybe qualified as M
import Data.Ord qualified as O
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace (trace)

import System.IO

main :: IO ()
main = do
  inp <- readFile "1.input"
  print (p1 inp 2)
  print (p1 inp 1000000)

data Grid = Grid
  { numRows :: Int
  , numCols :: Int
  , grid :: Map.Map (Int, Int) Char
  }
  deriving (Show)

p1 input costFactor =
  sum [cost p1 p2 | (p1, p2) <- pairs]
 where
  pairs = [(p1, p2) | (p1, x) <- zip idxs [0 ..], (p2, y) <- zip idxs [0 ..], x < y]
  g = parseGrid input
  emptyRows' = emptyRows g
  idxs = map fst $ Map.toList $ Map.filter (== '#') (grid g)
  (eRows, eCols) = (emptyRows g, emptyCols g)
  cost p1@(i, j) p2@(m, n) =
    abs (i - m) + abs (j - n) + numEmptyBetween * (costFactor - 1)
   where
    (minI, maxI) = (min i m, max i m)
    (minJ, maxJ) = (min j n, max j n)
    numEmptyRowsBetween = S.size $ S.intersection (S.fromList [minI .. maxI]) eRows
    numEmptyColsBetween = S.size $ S.intersection (S.fromList [minJ .. maxJ]) eCols
    numEmptyBetween = numEmptyRowsBetween + numEmptyColsBetween

parseGrid input =
  Grid
    { numRows = length rawLines
    , numCols = length $ head rawLines
    , grid = Map.fromList grid
    }
 where
  rawLines = lines input
  grid =
    [ ((i, j), c)
    | (i, line) <- zip [0 ..] rawLines
    , (j, c) <- zip [0 ..] line
    ]

emptyRows :: Grid -> S.Set Int
emptyRows g = S.fromList l
 where
  l =
    [ i
    | i <- [0 .. numRows g - 1]
    , all (\j -> Map.lookup (i, j) (grid g) & (== Just '.')) [0 .. numCols g - 1]
    ]

emptyCols :: Grid -> S.Set Int
emptyCols g = S.fromList l
 where
  l =
    [ j
    | j <- [0 .. numCols g - 1]
    , all (\i -> Map.lookup (i, j) (grid g) & (== Just '.')) [0 .. numRows g - 1]
    ]
