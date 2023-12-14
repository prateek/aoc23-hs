{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid reverse" #-}

module Main where

import Advent qualified
import Data.Char qualified as Char
import Data.List qualified as L
import Data.Map qualified as Map

import Data.List.Split qualified as S
import Data.Ord qualified as O
import System.IO ()

main :: IO ()
main = do
  input <- readFile "1.input"
  print (p1 $ parse input)
  print (p2 $ parse input)

parse :: String -> [String]
parse = lines

p1 :: [String] -> Int
p1 = load . tilt

load :: [[Char]] -> Int
load = sum . zipWith (*) [1 ..] . map (length . filter (== 'O')) . reverse

tilt :: [String] -> [String]
tilt g = tiltDir g 0

p2 :: [[Char]] -> Int
p2 init = load nth
  where
    -- detect how many applications from `init` till we hit a cycle
    (start, cycleLen) = detectCycle init 0 Map.empty
    targetCycles = 1_000_000_000
    howManyMore = start + mod (targetCycles - start) cycleLen
    nth = iterate fullTilt init !! howManyMore

detectCycle :: (Num a) => [[Char]] -> a -> Map.Map [[Char]] a -> (a, a)
detectCycle g count known =
  case Map.lookup g known of
    Just start -> (start, count - start)
    Nothing -> detectCycle (fullTilt g) (count + 1) (Map.insert g count known)

fullTilt :: [[Char]] -> [[Char]]
fullTilt grid = foldl tiltDir grid [0 .. 3]

tiltDir :: (Eq a, Num a) => [[Char]] -> a -> [[Char]]
tiltDir grid dir
  | dir == 0 = L.transpose $ map tiltL $ L.transpose grid
  | dir == 1 = map tiltL grid
  | dir == 2 = L.transpose $ map tiltR $ L.transpose grid
  | dir == 3 = map tiltR grid

tiltL = L.intercalate "#" . map (reverse . L.sort) . S.splitOn "#"
tiltR = reverse . tiltL . reverse
