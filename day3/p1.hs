#!/usr/bin/env runhaskell

import Data.Char qualified as Char
import Data.Maybe qualified as Maybe
import Debug.Trace (trace)
import System.IO

main :: IO ()
main = do
  input <- readFile "test.input"
  let inputLines = lines input
  -- print inputLines
  print (processInput inputLines)
  print (findNumbers ["a1twoone", "atwoone", "a1.321end0"])

findNumbers grid =
  concatMap (\(r, row) -> Maybe.catMaybes $ zipWith (parseNumber r) [0 ..] row) $ zip [0 ..] grid
  where
    parseNumber r c x
      | Char.isDigit x = Just (read [x] :: Int, (r, c))
      | otherwise = Nothing

processInput :: [String] -> Int
processInput [] = 0
processInput inLines = sumFinalNums
  where
    (numLines, lineLen) = (length inLines, minimum $ map length inLines)
    digitIdxs = find2DDigitIdxs inLines
    allNums = map (extractNum inLines) digitIdxs
    hasSurroundingSymbol (y, startX, endX) =
      let coords = candidateCoords y (startX, endX)
          filterCoords = filter (\(y', x') -> y' < numLines && x' < lineLen) coords
          surroundingChars = map (\(y', x') -> inLines !! y' !! x') filterCoords
       in any isGear surroundingChars
    finalCoords = filter hasSurroundingSymbol digitIdxs
    finalNums = map (extractNum inLines) finalCoords
    sumFinalNums = sum finalNums

isGear :: Char -> Bool
isGear char = not (Char.isDigit char || Char.isSpace char || char == '.')

extractNum :: [String] -> (Int, Int, Int) -> Int
extractNum inLines (y, startX, endX) =
  foldl extractNum' 0 [startX .. endX]
  where
    extractNum' num x = num * 10 + Char.digitToInt (inLines !! y !! x)

candidateCoords :: Int -> (Int, Int) -> [(Int, Int)]
candidateCoords y (startX, endX) =
  [ (y', x')
    | y' <- [y - 1 .. y + 1],
      y' >= 0,
      x' <- [startX - 1 .. endX + 1],
      x' >= 0
  ]

-- inputLines ->  [(y, startX, endX)]
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
