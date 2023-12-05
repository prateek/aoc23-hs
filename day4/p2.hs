#!/usr/bin/env runhaskell

import Data.Char qualified as Char
import Data.Set qualified as S
import Debug.Trace (trace)
import System.IO

main :: IO ()
main = do
   input <- readFile "test.input"
   let inputLines = lines input
   print $ processLines inputLines

processLines :: [String] -> Int
processLines inputLines =
   fst $ foldl go (0, repeat 1) winsPerCard
  where
   winsPerCard = map processLine inputLines
   go (oldTotal, numCurrCopies : oldCopies) numCurrWins =
      let newTotal = oldTotal + numCurrCopies
          deltaCopies = replicate numCurrWins numCurrCopies ++ repeat 0
          newCopies = zipWith (+) oldCopies deltaCopies
       in (newTotal, newCopies)

processLine :: String -> Int
processLine l =
   let (winNums, ourNums) = parseCard l
       numIntersect = S.size $ S.intersection winNums ourNums
    in numIntersect

-- >>> parseCard "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
-- ([41,48,83,86,17],[83,86,6,31,17,9,48,53])
parseCard :: String -> (S.Set Int, S.Set Int)
parseCard s =
   let (_, rest) = break (== ':') s
       (winNums, ourNums) = break (== '|') rest
       mapTuple f (a, b) = (f a, f b)
    in mapTuple (S.fromList . parseNums) (winNums, ourNums)

parseNums :: String -> [Int]
parseNums input =
   let filtered = filter (\c -> Char.isDigit c || Char.isSpace c) input
       tokens = words filtered
    in map read tokens
