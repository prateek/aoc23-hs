#!/usr/bin/env runhaskell

import Data.Char qualified as C
import Data.Function ((&))
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Maybe qualified as M
import Data.Ord qualified as O
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace (trace)

import System.IO

numSteps :: String -> Map.Map String (String, String) -> Int
numSteps steps graph =
  foldl lcm 1 numStepsEach
 where
  -- each start element looks like it cycles, so instead of computing the end state by hand
  -- we compute the num steps for each; and take the lcm
  numStepsEach = map (\x -> go [x] (cycle steps) graph) startStates

  go :: [String] -> [Char] -> Map.Map String (String, String) -> Int
  go current (s : rest) graph
    | allOnEnding current = 0
    | otherwise =
        if s /= 'L' && s /= 'R'
          then error "invalid step"
          else
            let next = map computeNext current
             in 1 + go next rest graph
   where
    allOnEnding = all (\x -> last x == 'Z')
    computeNext curr = case Map.lookup curr graph of
      Nothing -> error "no such key"
      Just (left, right) -> if s == 'L' then left else right

  startStates = Map.toList graph & map fst & filter (\x -> 'A' == last x)

splitSections :: String -> [String]
splitSections x = map T.unpack $ T.splitOn (T.pack "\n\n") $ T.pack x

parseInput :: String -> ([Char], Map.Map String (String, String))
parseInput input =
  let (transitions : rest) = splitSections input
   in (parseTransitions transitions, Map.fromList $ parseGraph $ head rest)

parseTransitions :: String -> [Char]
parseTransitions = filter (\x -> x == 'L' || x == 'R')

parseGraph :: String -> [(String, (String, String))]
parseGraph input = map parseLine $ lines input
 where
  parseLine line =
    let (key, value) = break (== '=') line
        trimmedKey = trim key
        trimmedValue = takeWhile (/= ')') . dropWhile (== '(') . dropWhile C.isSpace . tail $ value
        (val1, val2) = parseTuple trimmedValue
     in (trimmedKey, (val1, val2))

  parseTuple tupleString =
    let (v1, v2) = break (== ',') tupleString
     in (trim v1, trim $ tail v2)

  trim = f . f
   where
    f = reverse . dropWhile C.isSpace

-- Example usage
main :: IO ()
main = do
  input <- readFile "1.input"
  let (transitions, graph) = parseInput input
  -- let startState = Map.toList graph & map fst & filter (\x -> 'A' == last x)
  print (numSteps transitions graph)
