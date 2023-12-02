#!/usr/bin/env runhaskell
import Data.Maybe qualified as Maybe
import System.IO

firstVal line = firstNum line 0

firstNum :: String -> Int -> Maybe (Int, Int)
firstNum "" _ = Nothing
firstNum ('1' : _) offset = Just (1, offset)
firstNum ('o' : 'n' : 'e' : _) offset = Just (1, offset)
firstNum ('2' : _) offset = Just (2, offset)
firstNum ('t' : 'w' : 'o' : _) offset = Just (2, offset)
firstNum ('3' : _) offset = Just (3, offset)
firstNum ('t' : 'h' : 'r' : 'e' : 'e' : _) offset = Just (3, offset)
firstNum ('4' : _) offset = Just (4, offset)
firstNum ('f' : 'o' : 'u' : 'r' : _) offset = Just (4, offset)
firstNum ('5' : _) offset = Just (5, offset)
firstNum ('f' : 'i' : 'v' : 'e' : _) offset = Just (5, offset)
firstNum ('6' : _) offset = Just (6, offset)
firstNum ('s' : 'i' : 'x' : _) offset = Just (6, offset)
firstNum ('7' : _) offset = Just (7, offset)
firstNum ('s' : 'e' : 'v' : 'e' : 'n' : _) offset = Just (7, offset)
firstNum ('8' : _) offset = Just (8, offset)
firstNum ('e' : 'i' : 'g' : 'h' : 't' : _) offset = Just (8, offset)
firstNum ('9' : _) offset = Just (9, offset)
firstNum ('n' : 'i' : 'n' : 'e' : _) offset = Just (9, offset)
firstNum (_ : rest) offset = firstNum rest (offset + 1)

lastVal line = lastNum (reverse line) 0

lastNum :: String -> Int -> Maybe (Int, Int)
lastNum "" _ = Nothing
lastNum ('1' : _) offset = Just (1, offset)
lastNum ('e' : 'n' : 'o' : _) offset = Just (1, offset)
lastNum ('2' : _) offset = Just (2, offset)
lastNum ('o' : 'w' : 't' : _) offset = Just (2, offset)
lastNum ('3' : _) offset = Just (3, offset)
lastNum ('e' : 'e' : 'r' : 'h' : 't' : _) offset = Just (3, offset)
lastNum ('4' : _) offset = Just (4, offset)
lastNum ('r' : 'u' : 'o' : 'f' : _) offset = Just (4, offset)
lastNum ('5' : _) offset = Just (5, offset)
lastNum ('e' : 'v' : 'i' : 'f' : _) offset = Just (5, offset)
lastNum ('6' : _) offset = Just (6, offset)
lastNum ('x' : 'i' : 's' : _) offset = Just (6, offset)
lastNum ('7' : _) offset = Just (7, offset)
lastNum ('n' : 'e' : 'v' : 'e' : 's' : _) offset = Just (7, offset)
lastNum ('8' : _) offset = Just (8, offset)
lastNum ('t' : 'h' : 'g' : 'i' : 'e' : _) offset = Just (8, offset)
lastNum ('9' : _) offset = Just (9, offset)
lastNum ('e' : 'n' : 'i' : 'n' : _) offset = Just (9, offset)
lastNum (_ : rest) offset = lastNum rest (offset + 1)

processLine :: String -> Int
processLine line = 10 * a + b
  where
    (a, _) = Maybe.fromJust (firstVal line)
    (b, _) = Maybe.fromJust (lastVal line)

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
