#!/usr/bin/env runhaskell

import Data.Char qualified as Char
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Set qualified as S
import Debug.Trace (trace)
import System.IO

main :: IO ()
main = do
   input <- readFile "1.input"
   let inputLines = lines input
   print (sum $ map processLine inputLines)

-- >>> processLine "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
-- 8
processLine :: String -> Int
processLine l =
   let (winNums, ourNums) = parseCard l
       numIntersect = S.size $ S.intersection winNums ourNums
    in if numIntersect > 0 then 2 ^ (numIntersect - 1) else 0

-- >>> parseCard "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
-- ([41,48,83,86,17],[83,86,6,31,17,9,48,53])
parseCard :: String -> (S.Set Int, S.Set Int)
parseCard s =
   let (_, rest) = break (== ':') s
       (winNums, ourNums) = break (== '|') rest
       mapTuple f (a, b) = (f a, f b)
    in mapTuple (S.fromList . parseNums) (winNums, ourNums)

-- space separate numbers -> list of numbers
parseNums :: String -> [Int]
parseNums input =
   let filtered = filter (\c -> Char.isDigit c || Char.isSpace c) input
       tokens = words filtered
    in map read tokens
