#!/usr/bin/env runhaskell

import Data.Char qualified as Char
import Data.Maybe qualified as Maybe
import System.IO
import Text.Parsec qualified as Parsec
import Text.Parsec.String (Parser)

-- Define data types
data Color = Blue | Red | Green deriving (Show)

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

-- Function to find the Minset of a Round
data Minset = Minset {blue :: Int, red :: Int, green :: Int}

computeMinset :: Round -> Minset -> Minset
computeMinset [] accum = accum
computeMinset (x : rest) accum =
  case x of
    Item num Blue -> computeMinset rest (accum {blue = max (blue accum) num})
    Item num Red -> computeMinset rest (accum {red = max (red accum) num})
    Item num Green -> computeMinset rest (accum {green = max (green accum) num})

mergeMinsets :: [Minset] -> Minset
mergeMinsets [] = Minset 0 0 0
mergeMinsets (x : rest) =
  Minset
    (max (blue x) (blue (mergeMinsets rest)))
    (max (red x) (red (mergeMinsets rest)))
    (max (green x) (green (mergeMinsets rest)))

power :: Minset -> Int
power (Minset blue red green) = blue * red * green

-- Function to process each game
processGame :: Game -> Int
processGame (Game _ rounds) =
  power (mergeMinsets minsets)
  where
    minsets = map (\p -> computeMinset p (Minset 0 0 0)) rounds

-- Example usage
main :: IO ()
main = do
  -- Open the file
  handle <- openFile "1.input" ReadMode
  -- Read the file line by line
  input <- hGetContents handle

  case parseGames input of
    Left err -> print err
    Right games ->
      let powers = map processGame games
       in print (sum powers)

  -- Close the file
  hClose handle
