#!/usr/bin/env runhaskell
import System.IO

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord

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
  case firstNum(reverse line) of
    Nothing -> Nothing
    Just (num, offset)  -> Just (num, (length line) - 1 - offset)

numStrings :: [String]
numStrings = [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" ]
numValues :: [Int]
numValues = [1..9]

firstNumStringHelper :: String -> (String,Int) -> Int -> Maybe (Int, Int)
-- find the first `numString` in `line` and return the (num,index of numString in line)
firstNumStringHelper "" _ _ = Nothing
firstNumStringHelper line (numString, num) offset =
  if numString == (take (length numString) line)
  then Just (num, offset)
  else firstNumStringHelper (tail line) (numString, num) (offset + 1)

firstNumString :: String -> [(String,Int)] -> Maybe (Int, Int)
firstNumString line stringNums =
  case strOffsets of
    [] -> Nothing
    _ -> Just (List.minimumBy (Ord.comparing snd) strOffsets)
  where
    strOffsets = Maybe.mapMaybe (\p -> firstNumStringHelper line p 0) stringNums

-- | find the first numeric/string number in the line
--
-- Examples
--
-- >>> finalFirst "a1twoone"
-- Just (1,1)
--
-- >>> finalFirst "atwoone"
-- Just (2,1)
--
-- >>> finalFirst "sofone1two"
-- Just (1,3)
--

finalFirst line =
  let helper Nothing Nothing = Nothing
      helper a Nothing = a
      helper Nothing b = b
      helper a@(Just (a1, a2)) b@(Just (b1, b2)) =
        if a2 < b2
        then a
        else b
  in helper strNum regNum
  where
  strNum = firstNumString line (zip numStrings numValues)
  regNum = firstNum line

-- | find the last numeric/string number in the line
--
-- Examples
--
-- >>> finalSecond "1one"
-- Just (1,3)
--
-- >>> finalSecond "aone3two"
-- Just (2,7)
--
-- >>> finalSecond "sofone1two"
-- Just (2,9)
--

finalSecond line =
  case p of
    Nothing -> Nothing
    Just (num,offset) -> Just (num, (length line) - 1 - offset)
  where
    p = firstNumString (reverse line) (zip (map reverse numStrings) numValues)

-- main :: IO ()
-- main = do
--   print (firstNumStringHelper "soneomeoneone1" ("one",1) 0)
--   print (finalFirst "a1twoone")
--   print (finalSecond "atwoone")

processLine :: String -> Int
processLine line = 10 * a + b
  where
  (a, _) = Maybe.fromJust (finalFirst line)
  (b, _) = Maybe.fromJust (finalSecond line)

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
