#!/usr/bin/env runhaskell
import Data.Char qualified as Char
import System.IO

firstNum :: String -> Int
firstNum line =
  if Char.isDigit (head line)
    then Char.digitToInt (head line)
    else firstNum (tail line)

lastNum :: String -> Int
lastNum line = firstNum (reverse line)

processLine :: String -> Int
processLine line = 10 * firstNum line + lastNum line

main :: IO ()
main = do
  -- Open the file
  handle <- openFile "1.input" ReadMode

  -- Read the file line by line
  contents <- hGetContents handle

  -- sum of the nums
  let s = sum (map processLine (lines contents))
  print s

  -- Close the file
  hClose handle
