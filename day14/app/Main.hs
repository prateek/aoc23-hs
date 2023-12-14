{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Advent qualified
import Data.Char qualified as Char
import Data.Either qualified
import Data.Function ((&))
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Ord qualified as O
import Data.Set qualified as S
import Data.Text qualified
import Debug.Trace (trace)
import System.Environment (lookupEnv)
import System.IO ()

main :: IO ()
main = do
  input <- readFile "1.input"
  print (p1 $ parse input)
  print (p2 $ parse input)

p2 :: [[Char]] -> Int
p2 init = load nth
  where
    -- detect how many applications from `init` till we hit a cycle
    (start, cycleLen, _) = detectCycle init 0 Map.empty
    targetCycles = 1_000_000_000
    howManyMore = start + mod (targetCycles - start) cycleLen
    nth = iterate fullTilt init !! howManyMore

parse :: String -> [String]
parse = lines

detectCycle :: (Num a) => [[Char]] -> a -> Map.Map [[Char]] a -> (a, a, [[Char]])
detectCycle g count known =
  case Map.lookup g known of
    Just start -> (start, count - start, g)
    Nothing -> detectCycle (fullTilt g) (count + 1) (Map.insert g count known)

p1 :: [String] -> Int
p1 = load . tilt

load :: [[Char]] -> Int
load grid =
  sum . zipWith (*) revLengths $ map (length . filter (== 'O')) grid
  where
    n = length $ head grid
    revLengths = reverse [0 .. n]

tilt :: [String] -> [String]
tilt g = tiltDir g 0

fullTilt :: [[Char]] -> [[Char]]
fullTilt grid = foldl tiltDir grid [0 .. 3]

tiltDir :: (Eq a, Num a) => [[Char]] -> a -> [[Char]]
tiltDir grid dir
  | dir == 0 = L.transpose $ map (tiltRow True) $ L.transpose grid
  | dir == 1 = map (tiltRow True) grid
  | dir == 2 = L.transpose $ map (tiltRow False) $ L.transpose grid
  | dir == 3 = map (tiltRow False) grid

tiltRow :: Bool -> [Char] -> [Char]
tiltRow front row
  | front = go ([], [], []) row
  | otherwise = reverse (go ([], [], []) (reverse row))
  where
    go (front, pendingO, pendingP) [] = front ++ pendingO ++ pendingP
    go accum@(front, pendingO, pendingP) (r : rs) =
      case r of
        '.' -> go (front, pendingO, r : pendingP) rs
        'O' -> go (front, r : pendingO, pendingP) rs
        '#' -> go accum [] ++ '#' : go ([], [], []) rs
