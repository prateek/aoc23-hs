#!/usr/bin/env runhaskell

import Data.Char qualified as Char
import Data.Function ((&))
import Data.List qualified as L
import Data.Maybe qualified as M
import Data.Ord qualified as O
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace (trace)

import System.IO

data Range = Range
  { destination :: Int
  , source :: Int
  , len :: Int
  }
  deriving (Show, Eq, Ord)

type Seeds = [Int]
type StepMap = [Range]

group2 :: [a] -> [(a, a)]
group2 [] = []
group2 [_] = []
group2 (x : y : xs) = (x, y) : group2 xs

toDest :: Range -> Int -> Int
toDest r s = destination r + s - source r

inRange :: Range -> Int -> Bool
inRange r s = s >= source r && s < source r + len r

srcToDest :: [Range] -> Int -> Int
srcToDest [] src = src
srcToDest (r : rs) src =
  if inRange r src
    then toDest r src
    else srcToDest rs src

parseSeeds1 :: String -> Seeds
parseSeeds1 = map read . words . drop 6

parseStepMap :: String -> StepMap
parseStepMap = map parseRange . drop 1 . lines
 where
  parseRange line = let [d, s, l] = map read $ words line in Range d s l

splitSections :: String -> [String]
splitSections x = map T.unpack $ T.splitOn (T.pack "\n\n") $ T.pack x

data Loc = Loc Int Int -- startIncl, endExcl
  deriving (Show, Eq, Ord)

data MappedLoc = MappedLoc Int Loc Loc -- source, destination
  deriving (Show, Eq, Ord)

intersects :: Loc -> Loc -> Bool
intersects (Loc s0 e0) (Loc s1 e1) = e0 > s1 && s0 < e1

contains :: Loc -> Loc -> Bool
contains (Loc s0 e0) (Loc s1 e1) = s0 <= s1 && e0 >= e1

intersection :: Loc -> Loc -> Maybe Loc
intersection p0@(Loc s0 e0) p1@(Loc s1 e1)
  | intersects p0 p1 = Just (Loc (max s0 s1) (min e0 e1))
  | otherwise = Nothing

difference :: Loc -> Loc -> [Loc]
difference p0@(Loc s0 e0) p1@(Loc s1 e1)
  | not (intersects p0 p1) = [p0]
  | s0 < s1 && e0 > e1 = [Loc s0 s1, Loc e1 e0]
  | s0 < s1 = [Loc s0 s1]
  | e0 > e1 = [Loc e1 e0]
  | otherwise = []

applyStepMap :: Int -> Loc -> StepMap -> [MappedLoc]
applyStepMap l s [] = [MappedLoc l s s]
applyStepMap l s (r : rs) =
  mappedCommon ++ mappedDiffed
 where
  common = intersection s (Loc (source r) (source r + len r))
  mappedCommon = case common of
    Nothing -> []
    Just (Loc s0 e0) ->
      let s0' = toDest r s0
       in [MappedLoc l (Loc s0 e0) (Loc s0' (s0' + e0 - s0))]
  diffed = difference s (Loc (source r) (source r + len r))
  mappedDiffed = concatMap (\d -> applyStepMap l d rs) diffed

applyStepMaps :: [StepMap] -> Loc -> [(Loc, [MappedLoc])]
applyStepMaps stepMaps loc =
  foldl go [(loc, [])] (zip [0 ..] stepMaps)
 where
  go :: [(Loc, [MappedLoc])] -> (Int, StepMap) -> [(Loc, [MappedLoc])]
  go acc (l, stepMap) = concatMap (go' (l, stepMap)) acc
  go' :: (Int, StepMap) -> (Loc, [MappedLoc]) -> [(Loc, [MappedLoc])]
  go' (l, stepMap) (loc, mappedLocs) =
    map (\mLoc@(MappedLoc _ _ dst) -> (dst, mLoc : mappedLocs)) applied
   where
    applied = applyStepMap l loc stepMap

p2 input =
  let (locs, stepMaps) = parseInput2 input
      finalLocations = concatMap (applyStepMaps stepMaps) locs
   in L.minimumBy (O.comparing fst) finalLocations

parseInput2 :: String -> ([Loc], [StepMap])
parseInput2 s =
  let (seeds : rawStepMaps) = splitSections s
      stepMaps = map parseStepMap rawStepMaps
      seedRanges = parseSeeds1 seeds & seeds1ToRange
   in (seedRanges, stepMaps)

seeds1ToRange :: Seeds -> [Loc]
seeds1ToRange x = group2 x & map (\(s, l) -> Loc s (s + l))

main :: IO ()
main = do
  input <- readFile "1.input"
  let (minRange, mlocs) = p2 input
  print minRange
  print (map (\o@(MappedLoc _ _ (Loc start end)) -> show o ++ " " ++ show (end - start)) mlocs)
