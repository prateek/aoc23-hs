#!/usr/bin/env runhaskell

import Data.Char qualified as Char
import Data.Maybe qualified as Maybe
import System.IO
import Text.Parsec qualified as Parsec
import Text.Parsec.String (Parser)

-- Define data types
data Color = Blue | Red | Green deriving (Eq, Show)

data Item = Item Int Color deriving (Show)

type Round = [Item]

type GameID = Int

data Game = Game GameID [Round] deriving (Show)

-- Parser for a number
numberParser :: Parser Int
numberParser = read <$> Parsec.many1 Parsec.digit

-- Parser for a color
colorParser :: Parser Color
colorParser = do
  color <- Parsec.many1 Parsec.letter
  return $ case map Char.toLower color of
    "blue" -> Blue
    "red" -> Red
    "green" -> Green
    _ -> error "Invalid color"

-- Parser for an item (number and color)
itemParser :: Parser Item
itemParser = do
  num <- numberParser
  Parsec.space
  Item num <$> colorParser

-- Parser for a game part (list of items)
roundParser :: Parser Round
roundParser = itemParser `Parsec.sepBy` Parsec.string ", "

-- Parser for the game identifier
gameIDParser :: Parser GameID
gameIDParser = do
  Parsec.string "Game "
  id <- numberParser
  Parsec.string ": "
  return id

-- Parser for a game (game identifier and list of game rounds)
gameParser :: Parser Game
gameParser = do
  gameId <- gameIDParser
  rounds <- roundParser `Parsec.sepBy` Parsec.string "; "
  return $ Game gameId rounds

-- Parser for multiple games
gamesParser :: Parser [Game]
gamesParser = gameParser `Parsec.endBy` Parsec.newline

-- Parse a string
parseGames :: String -> Either Parsec.ParseError [Game]
parseGames = Parsec.parse gamesParser ""

data Minset = Minset {blue :: Int, red :: Int, green :: Int}

-- Function to find the Minset of a Round
computeMinset :: Round -> Minset
computeMinset = foldl merge (Minset 0 0 0)
  where
    merge accum item = mergeMinsets [accum, itemToMinset item]

itemToMinset :: Item -> Minset
itemToMinset (Item num color)
  | color == Blue = Minset num 0 0
  | color == Red = Minset 0 num 0
  | color == Green = Minset 0 0 num

mergeMinsets :: [Minset] -> Minset
mergeMinsets = foldl merge (Minset 0 0 0)
  where
    -- TODO: i really don't like records in haskell. is there a better way to do this?
    -- for instance if this was a list instead, i could zipWith
    merge (Minset b1 r1 g1) (Minset b2 r2 g2) = Minset (max b1 b2) (max r1 r2) (max g1 g2)

power :: Minset -> Int
power (Minset b r g) = b * r * g

-- Function to process each game
processGame :: Game -> Int
processGame (Game _ rounds) =
  let minsets = map computeMinset rounds
   in power $ mergeMinsets minsets

-- Example usage
main :: IO ()
main = do
  -- Open the file
  handle <- openFile "1.input" ReadMode
  -- Read the file line by line
  input <- hGetContents handle

  case parseGames input of
    Left err -> print err
    Right games -> do
      let powers = map processGame games
      print $ sum powers

  -- Close the file
  hClose handle
