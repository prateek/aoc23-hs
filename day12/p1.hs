{- cabal:
build-depends: base
             , split
             ,
-}

import Data.Char qualified as Char
import Data.Function ((&))
import Data.List qualified as L
import Data.Map qualified as M
import Data.Ord qualified as O
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace (trace, traceShow)

import System.IO

-- ? unknown
-- . operational
-- # damaged

-- exercise after the problem:
-- i wanted to append [0,0...] at the end of pending blocks
-- could i have done that and memo-ized still?

-- before processing
-- append '.' at the end of input string

-- -- numValid row runningDamaged pendingBlocks = undefined
-- numValid :: String -> Int -> [Int] -> Int
-- numValid row running pendingBlocks =
--   numValid' row running pendingBlocks
--  where
--   memo =
--     M.fromList
--       [ ((rs, val, bl), numValid' rs val bl)
--       | rs <- L.tails row
--       , val <- [0 .. max pendingBlocks]
--       , bl <- L.tails pendingBlocks
--       ]
--   go a b c = memo M.! (a, b, c)
--   numValid' "" 0 [] = 1
--   -- numValid "" x [y] = if x == y then 1 else 0 -- will this ever be hit?
--   numValid' "" _ _ = 0
--   numValid' ('.' : rr) n [] = if n == 0 then numValid' rr 0 [] else 0
--   numValid' ('.' : rr) n bl@(b : bs)
--     | n == b = numValid' rr 0 bs
--     | n == 0 = numValid' rr 0 bl
--     | otherwise = 0
--   numValid' ('#' : rr) n bl = numValid' rr (n + 1) bl
--   numValid' ('?' : rr) n bl = numValid' rr (n + 1) bl + numValid' ('.' : rr) n bl
--   numValid' p _ _ = error p

numValid' "" 0 [] = 1
numValid' "" _ _ = 0
numValid' ('.' : rr) n bl
  | isFirst n bl = numValid' rr 0 (tail bl)
  | n == 0 = numValid' rr 0 bl
  | otherwise = 0
numValid' ('#' : rr) n bl = numValid' rr (n + 1) bl
numValid' ('?' : rr) n bl =
  numValid' rr (n + 1) bl
    + if isFirst n bl
      then numValid' rr 0 (tail bl)
      else
        if n == 0
          then numValid' rr 0 bl
          else 0
numValid' p _ _ = error p

isFirst x (b : bs) = x == b
isFirst _ _ = False

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

main :: IO ()
main = do
  print "Hello"
