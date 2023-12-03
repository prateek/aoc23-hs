#!/usr/bin/env runhaskell

import Data.Char qualified as Char
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Debug.Trace (trace)
import System.IO

main :: IO ()
main = do
  input <- readFile "1.input"
  let inputLines = lines input
      numLines = length inputLines
  -- print inputLines
  print (processInput inputLines)

processInput :: [String] -> Int
processInput [] = 0
processInput inLines = sum gearRatios
  where
    maxBounds = (length inLines, minimum $ map length inLines)
    numCoords = find2DDigitIdxs inLines
    starCoords = findStars inLines
    starNumCoordPairs = map (\coord -> (coord, findSurroundingNums maxBounds numCoords coord)) starCoords
    gearCoords = filter (\(_, coords) -> 2 == length coords) starNumCoordPairs
    gearRatios = map (\(_, coords) -> product $ map (extractNum inLines) coords) gearCoords

findSurroundingNums maxBounds numCoords (y, x) =
  let coordsAroundYX = candidateCoords maxBounds y (x, x)
      isCloseToYX (y', x'Start, x'End) = any (\(l, m) -> y' == l && x'Start <= m && m <= x'End) coordsAroundYX
      surroundingNums = filter isCloseToYX numCoords
      dedupedNums = List.nub surroundingNums
   in dedupedNums

findStars :: [String] -> [(Int, Int)]
findStars inLines =
  let allCoords = zip [0 ..] (map (zip [0 ..]) inLines)
      flattened = concatMap (\(y, row) -> map (\(x, char) -> (y, x, char)) row) allCoords
      starCoords = filter (\(_, _, char) -> isStar char) flattened
   in map (\(y, x, _) -> (y, x)) starCoords

isStar :: Char -> Bool
isStar char = char == '*'

extractNum :: [String] -> (Int, Int, Int) -> Int
extractNum inLines (y, startX, endX) =
  foldl extractNum' 0 [startX .. endX]
  where
    extractNum' num x = num * 10 + Char.digitToInt (inLines !! y !! x)

candidateCoords :: (Int, Int) -> Int -> (Int, Int) -> [(Int, Int)]
candidateCoords (maxY, maxX) y (startX, endX) =
  [ (y', x')
    | y' <- [y - 1 .. y + 1],
      y' >= 0,
      y' < maxY,
      x' <- [startX - 1 .. endX + 1],
      x' >= 0,
      x' < maxX
  ]

-- given 2d string, return coordinates of all contigous digits [(y, startX, endX)]
-- nb: output is sorted by y first, then by startX
-- >>> find2DDigitIdxs ["a1twoone", "atwoone", "a1.321end0"]
-- [(0,1,1),(2,1,1),(2,3,5),(2,9,9)]
--
find2DDigitIdxs :: [String] -> [(Int, Int, Int)]
find2DDigitIdxs inLines =
  let lineIdxs = zip inLines [0 ..]
      lineDigits = map (\(line, idx) -> (idx, findDigitIdxs line)) lineIdxs
      nonEmpty = filter (\(_, digits) -> not $ null digits) lineDigits
      flattened = concatMap (\(y, idxs) -> map (\(startX, endX) -> (y, startX, endX)) idxs) nonEmpty
   in flattened

-- >>> findDigitIdxs "a1twoone"
-- [(1,1)]
-- >>> findDigitIdxs "atwoone"
-- []
-- >>> findDigitIdxs "a1.321end0"
-- [(1,1),(3,5),(9,9)]

findDigitIdxs :: String -> [(Int, Int)]
findDigitIdxs line =
  let charIdxs = zip line [0 ..]
      digitIdxs = foldl findDigitIdxs' (NoActive []) charIdxs
   in case digitIdxs of
        NoActive idxs -> reverse idxs
        Active start idxs -> reverse $ (start, length line - 1) : idxs
  where
    findDigitIdxs' :: DigitAccum -> (Char, Int) -> DigitAccum
    findDigitIdxs' original@(NoActive idxs) (char, idx)
      | Char.isDigit char = Active idx idxs
      | otherwise = original
    findDigitIdxs' original@(Active start idxs) (char, idx)
      | Char.isDigit char = original
      | otherwise = NoActive ((start, idx - 1) : idxs)

data DigitAccum
  = NoActive [(Int, Int)] -- list of incl (start, end) digit idxs
  | Active Int [(Int, Int)] -- active start digit idx, list of incl (start, end) digit idxs
