#!/usr/bin/env runhaskell
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Ord qualified as Ord
import System.IO

firstNum :: String -> Maybe (Int, Int)
firstNum line = firstNumHelper line 0

firstNumHelper :: String -> Int -> Maybe (Int, Int)
firstNumHelper "" _ = Nothing
firstNumHelper line offset =
  if Char.isDigit (head line)
    then Just (Char.digitToInt (head line), offset)
    else firstNumHelper (tail line) (offset + 1)

lastNum :: String -> Maybe (Int, Int)
lastNum line =
  case firstNum (reverse line) of
    Nothing -> Nothing
    Just (num, offset) -> Just (num, (length line) - 1 - offset)

numStrings :: [String]
numStrings = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

numValues :: [Int]
numValues = [1 .. 9]

data OffsetT = OffsetT {idx :: Int, strValue :: String, numValue :: Int}

-- find the first `numString` in `line` and return the (offset, string, numerical value)
firstNumStringHelper :: String -> (String, Int) -> Int -> Maybe OffsetT
firstNumStringHelper "" _ _ = Nothing
firstNumStringHelper line (numString, num) offset =
  if numString == (take (length numString) line)
    then Just OffsetT {idx = offset, strValue = numString, numValue = num}
    else firstNumStringHelper (tail line) (numString, num) (offset + 1)

firstNumString :: String -> [(String, Int)] -> [OffsetT]
firstNumString line stringNums =
  Maybe.mapMaybe (\p -> firstNumStringHelper line p 0) stringNums

-- | find the first numeric/string number in the line
--
-- Examples
--
-- >>> finalFirst "a1twoone"
-- (1,1)
--
-- >>> finalFirst "atwoone"
-- (2,1)
--
-- >>> finalFirst "sofone1two"
-- (1,3)
finalFirst line =
  List.minimumBy (Ord.comparing snd) offsets
  where
    numOffset = firstNum line
    strOffsetTs = firstNumString line (zip numStrings numValues)
    strOffsets = map (\x -> (numValue x, idx x)) strOffsetTs
    offsets = strOffsets ++ (Maybe.maybeToList numOffset)

-- | find the last numeric/string number in the line
--
-- Examples
--
-- >>> finalSecond "2seven"
-- (7,1)
--
-- >>> finalSecond "4nineeightseven2"
-- (2,15)
--
-- >>> finalSecond "4nineeight2seven"
-- (7,11)
finalSecond line =
  let (num, offset) = List.minimumBy (Ord.comparing snd) offsets
   in (num, (length line) - 1 - offset)
  where
    numOffset = firstNum (reverse line)
    strOffsetTs = firstNumString (reverse line) (zip (map reverse numStrings) numValues)
    strOffsets = map (\x -> (numValue x, (idx x) + (length (strValue x)) - 1)) strOffsetTs
    offsets = strOffsets ++ (Maybe.maybeToList numOffset)

processLine :: String -> Int
processLine line = 10 * fst (finalFirst line) + fst (finalSecond line)

main :: IO ()
main = do
  -- Open the file
  handle <- openFile "1.input" ReadMode

  -- Read the file line by line
  contents <- hGetContents handle

  -- sum of the nums
  let s = sum (map processLine (lines contents))
  putStrLn (show s)

  -- Close the file
  hClose handle
