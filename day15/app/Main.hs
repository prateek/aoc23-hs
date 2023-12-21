{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Char qualified as C
import Data.Char qualified as Char
import Data.Function ((&))
import Data.List qualified as L
import Data.List.Split qualified as S
import Data.Map qualified as Map
import Data.Maybe qualified as M
import Data.Ord qualified as O
import Data.Text qualified
import Debug.Trace (trace)
import System.IO ()

mkBoxes :: Map.Map Int [Step]
mkBoxes = Map.fromList [(i, []) | i <- [0 .. 255]]

main :: IO ()
main = do
  test <- readFile "test.input"
  inp <- readFile "1.input"
  print (p1 inp)
  print ((focalPower . p2') inp)

data Step = Equals String Int | Sub String
  deriving (Eq, Show)

parseStep :: String -> Step
parseStep s
  | '-' `L.elem` s = Sub (head $ S.splitOn "-" s)
  | '=' `L.elem` s = Equals (head $ S.splitOn "=" s) (read $ S.splitOn "=" s !! 1)

focalPower :: Map.Map Int [Step] -> Int
focalPower m = sum powers
  where
    powers = map (\(k, steps) -> (1 + k) * go steps) (Map.assocs m)
    go steps =
      sum . zipWith (*) [1 ..] $ map (\(Equals k v) -> v) steps

p2' row = go boxes (steps row)
  where
    boxes = mkBoxes
    steps = map parseStep . S.splitOn ","
    go boxes [] = boxes
    go boxes (Sub p : xs) = go boxes' xs
      where
        h = hash1 p
        curr = boxes Map.! h
        boxes' = Map.insert h (removeStep curr p) boxes
    go boxes (Equals ptr val : xs) = go boxes' xs
      where
        h = hash1 ptr
        curr = boxes Map.! h
        boxes' = Map.insert h (upsertStep curr (Equals ptr val)) boxes

upsertStep [] o@(Equals ptr val) = [o]
upsertStep (o@(Equals ptr val) : xs) o'@(Equals k v)
  | ptr == k = o' : xs
  | otherwise = o : upsertStep xs o'

removeStep :: [Step] -> String -> [Step]
removeStep [] _ = []
removeStep (o@(Equals ptr _) : xs) p
  | ptr == p = xs
  | otherwise = o : removeStep xs p

p1 = sum . map hash1 . S.splitOn ","

hash1 :: String -> Int
hash1 = foldl go 0
  where
    go acc char = (17 * (acc + Char.ord char)) `mod` 256
