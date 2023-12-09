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

numSteps steps graph =
  go "AAA" (cycle steps) graph
 where
  go :: String -> [Char] -> Map.Map String (String, String) -> Int
  go "ZZZ" _ _ = 0
  go current (s : rest) graph =
    if s /= 'L' && s /= 'R'
      then error "invalid step"
      else case Map.lookup current graph of
        Nothing -> error "no such key"
        Just (left, right) ->
          1
            +
            -- + trace (show current ++ " " ++ show s ++ " " ++ show left ++ " " ++ show right)
            ( if s == 'L'
                then go left rest graph
                else go right rest graph
            )

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
  print (numSteps transitions graph)
