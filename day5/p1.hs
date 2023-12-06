#!/usr/bin/env runhaskell

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}

import Data.Char qualified as Char
import Data.Foldable qualified as L
import Data.Function ((&))
import Data.List qualified as L
import Data.Maybe qualified as Maybe
import Data.Ord qualified as O
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace (trace)

import System.IO

data Range = Range
  { destination :: Int
  , source :: Int
  , len :: Int
  }
  deriving (Show, Eq, Ord)

type Seeds = [Int]
type StepMap = [Range]

-- assumes `inRange s`
toDest :: Range -> Int -> Int
toDest r s = destination r + s - source r

inRange :: Range -> Int -> Bool
inRange r s = s >= source r && s < source r + len r

srcToDest :: [Range] -> Int -> Int
srcToDest [] src = src
srcToDest (r : rs) src =
  if inRange r src
    then toDest r src
    else srcToDest rs src

parseSeeds1 :: String -> Seeds
parseSeeds1 = map read . words . drop 6

parseStepMap :: String -> StepMap
parseStepMap = map parseRange . drop 1 . lines
 where
  parseRange line = let [d, s, l] = map read $ words line in Range d s l

-- Helper function to split the content into sections
-- >>> splitSections "hello\n\nworld"
-- ["hello","world"]
splitSections :: String -> [String]
splitSections x = map T.unpack $ T.splitOn (T.pack "\n\n") $ T.pack x

parseInput1 :: String -> (Seeds, [StepMap])
parseInput1 s =
  let (seeds : stepMaps) = splitSections s
   in (parseSeeds1 seeds, map parseStepMap stepMaps)

mapSeedsToLocation :: (Seeds, [StepMap]) -> [Int]
mapSeedsToLocation (seeds, stepMaps) =
  map (\s -> foldl (\x seedMap -> srcToDest seedMap x) s stepMaps) seeds

p1 :: String -> Int
p1 = minimum . mapSeedsToLocation . parseInput1

main :: IO ()
main = do
  input <- readFile "1.input"
  print (p1 input)
