{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Array
import Data.Bits
import Data.Char
import Data.Function
import Data.List
import Data.Maybe (Maybe, fromJust, isJust, isNothing)

direction :: [(Int, Int)]
direction = [(-1, 0), (0, 1), (1, 0), (0, -1)]

toDirection :: Char -> (Int, Int)
toDirection c = case c of
  '|' -> (0, 2)
  '-' -> (1, 3)
  'L' -> (0, 1)
  'J' -> (0, 3)
  '7' -> (2, 3)
  'F' -> (1, 2)
  _ -> (-1, -1)

otherDirection :: Char -> Int -> Int
otherDirection c d
  | f == d = s
  | s == d = f
  | otherwise = -1
  where
    (f, s) = toDirection c

startPos :: [String] -> (Int, Int)
startPos grid = (x, y)
  where
    x = fromJust . findIndex (\s -> 'S' `elem` s) $ grid
    y = fromJust . elemIndex 'S' $ grid !! x

moveCell :: Int -> (Int, Int) -> (Int, Int)
moveCell dir (x, y) = let (dx, dy) = direction !! dir in (x + dx, y + dy)

oppositeDirection :: Int -> Int = xor 2

getPath :: Char -> [String] -> (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
getPath fi grid (sx, sy) (tx, ty) dir
  | not (inRange (0, m - 1) sx) || not (inRange (0, n - 1) sy) = []
  | sx == tx && sy == ty && dir /= -1 = [(sx, sy)]
  | otherwise = if od == -1 || null recr then [] else (sx, sy) : recr
  where
    m = length grid
    n = length $ head grid
    ch = grid !! sx !! sy
    od = if dir == -1 then fst (toDirection fi) else otherDirection ch (oppositeDirection dir)
    recr = getPath fi grid (moveCell od (sx, sy)) (tx, ty) od

getLoop :: [String] -> [(Int, Int)]
getLoop grid = go ans
  where
    (sx, sy) = startPos grid
    go c = getPath c grid (sx, sy) (sx, sy) (-1)
    ans = fromJust $ find (not . null . go) "|-LJ7F"

shoeLace :: [(Int, Int)] -> Int
shoeLace [_] = 0
shoeLace ((x1, y1) : (x2, y2) : xs) = (y1 + y2) * (x2 - x1) + shoeLace ((x2, y2) : xs)

part1 :: [String] -> Int
part1 grid = length (getLoop grid) `div` 2

part2 :: [String] -> Int
part2 grid =
  --- pick's theorem
  let path = getLoop grid
   in (abs (shoeLace path) - length path + 3) `div` 2

main :: IO ()
main = do
  strings <- lines <$> getContents
  print $ part1 strings
  print $ part2 strings
