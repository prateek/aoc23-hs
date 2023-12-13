#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , split
             , text
             , containers
default-extensions:
  ImportQualifiedPost
-}

import Data.Char qualified as Char
import Data.Function ((&))
import Data.List qualified as L
import Data.List.Split (splitOn)
import Data.Map qualified as M
import Data.Maybe qualified as Maybe
import Data.Ord qualified as O
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace (trace, traceShow)

import System.IO

--
-- >>> numValid "." 0 []
-- 1
-- >>> numValid "..." 0 []
-- 1
-- >>> numValid "..." 0 [1]
-- 0
-- >>> numValid "#." 0 []
-- 0
-- >>> numValid "#." 0 [1]
-- 1
-- >>> numValid ".#." 0 [1]
-- 1
-- >>> numValid "???.###." 0 [1,1,3]
-- 1
-- >>> numValid "?###????????." 0 [3,2,1]
-- 10

memoizeValid running row pendingBlocks = numValid' row running pendingBlocks
 where
  memo = M.fromList [((rs, val, bl), numValid rs val bl) | rs <- L.tails row, val <- [0 .. (maximum pendingBlocks)], bl <- L.tails pendingBlocks]

  numValid' a b c = Maybe.fromMaybe 0 (M.lookup (a, b, c) memo) -- error ("not found" ++ show (a, b, c))
  numValid "" 0 [] = 1
  numValid "" _ _ = 0
  numValid ('.' : rr) 0 [] = numValid' rr 0 []
  numValid ('.' : rr) _ [] = 0
  numValid ('.' : rr) n bl@(b : bs)
    | n == b = numValid' rr 0 bs
    | n == 0 = numValid' rr 0 bl
    | otherwise = 0
  numValid ('#' : rr) n bl = numValid' rr (n + 1) bl
  -- nb: need to use numValid, not numValid' here as the former is not memoized for these inputs
  --     i could express this recurrence in terms of numValid' but I end up having to repeat
  --     the logic
  numValid ('?' : rr) n bl = numValid ('#' : rr) n bl + numValid ('.' : rr) n bl
  -- numValid p _ _ = error p

  isFirst n (h : _) = n == h
  isFirst _ _ = False

parse :: String -> (String, [Int])
parse xs =
  let [word, cont] = words xs
   in (word, map read $ splitOn "," cont)

-- append "." to make edge cases easier to reason about
preprocess :: (String, [Int]) -> (String, [Int])
preprocess (xs, ys) = (xs ++ ".", ys)

-- heavily inspired by https://github.com/ColonelPhantom/aoc2023/blob/main/Day12.hs
unfold :: (String, [Int]) -> (String, [Int])
unfold (xs, ys) = (L.intercalate "?" (replicate 5 xs), concat $ replicate 5 ys)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "1.input"
  let v = map (uncurry (memoizeValid 0) . preprocess) input
  print ("p1: ", sum v)

  let v2 = map (uncurry (memoizeValid 0) . preprocess . unfold) input
  print ("p2: ", sum v2)
