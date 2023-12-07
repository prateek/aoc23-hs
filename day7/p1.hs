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

data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Enum, Show, Read)

toCardValue :: Char -> CardValue
toCardValue '2' = Two
toCardValue '3' = Three
toCardValue '4' = Four
toCardValue '5' = Five
toCardValue '6' = Six
toCardValue '7' = Seven
toCardValue '8' = Eight
toCardValue '9' = Nine
toCardValue 'T' = Ten
toCardValue 'J' = Jack
toCardValue 'Q' = Queen
toCardValue 'K' = King
toCardValue 'A' = Ace
toCardValue _ = error "Invalid card value"

instance Ord CardValue where
  compare = O.comparing (fromEnum :: CardValue -> Int)

instance Ord Hand where
  compare = O.comparing (fromEnum :: Hand -> Int)

data Hand
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Eq, Enum, Show, Read)

data FullHand = FullHand Hand [CardValue] deriving (Eq, Ord, Show)

toHand :: String -> FullHand
toHand str =
  let cards = map toCardValue str
      sorted = L.sortOn O.Down cards & L.group
      grouped = L.sortOn (O.Down . length) sorted
      values = map head grouped
      counts = map length grouped
   in case counts of
        [5] -> FullHand FiveOfAKind cards
        [4, 1] -> FullHand FourOfAKind cards
        [3, 2] -> FullHand FullHouse cards
        [3, 1, 1] -> FullHand ThreeOfAKind cards
        [2, 2, 1] -> FullHand TwoPair cards
        [2, 1, 1, 1] -> FullHand OnePair cards
        [1, 1, 1, 1, 1] -> FullHand HighCard cards
        _ -> error "Invalid hand"

parseLine :: String -> (FullHand, Int)
parseLine line =
  let (str : rest) = words line
      hand = toHand str
      player = read $ head rest
   in (hand, player)

parseLines :: String -> [(FullHand, Int)]
parseLines = map parseLine . lines

p1 input =
  sum (zipWith (*) [1 ..] (map snd sorted))
 where
  l = parseLines input
  sorted = L.sort l

main :: IO ()
main = do
  -- input <- readFile "test.input"
  input <- readFile "1.input"
  print (p1 input)
