#!/usr/bin/env runhaskell
import Data.Char qualified as Char
import Data.Maybe qualified as Maybe
import System.IO
import Text.Parsec qualified as Parsec
import Text.Parsec.String (Parser)

-- Define data types
data Color = Blue | Red | Green deriving (Show)

data Item = Item Int Color deriving (Show)

type GamePart = [Item]

type GameID = Int

data Game = Game GameID [GamePart] deriving (Show)

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
  color <- colorParser
  return $ Item num color

-- Parser for a game part (list of items)
gamePartParser :: Parser GamePart
gamePartParser = itemParser `Parsec.sepBy` Parsec.string ", "

-- Parser for the game identifier
gameIDParser :: Parser GameID
gameIDParser = do
  Parsec.string "Game "
  id <- numberParser
  Parsec.string ": "
  return id

-- Parser for a game (game identifier and list of game parts)
gameParser :: Parser Game
gameParser = do
  gameId <- gameIDParser
  parts <- gamePartParser `Parsec.sepBy` Parsec.string "; "
  return $ Game gameId parts

-- Parser for multiple games
gamesParser :: Parser [Game]
gamesParser = gameParser `Parsec.endBy` Parsec.newline

-- Parse a string
parseGames :: String -> Either Parsec.ParseError [Game]
parseGames input = Parsec.parse gamesParser "" input

-- Function to validate a GamePart
validateGamePart :: GamePart -> Bool
validateGamePart [] = True
validateGamePart (Item count Blue : rest) = count <= 14 && validateGamePart rest
validateGamePart (Item count Green : rest) = count <= 13 && validateGamePart rest
validateGamePart (Item count Red : rest) = count <= 12 && validateGamePart rest

-- Function to process each game
processGame :: Game -> Maybe Int
processGame (Game id parts) =
  if all (== True) validParts
    then Just id
    else Nothing
  where
    validParts = map validateGamePart parts

-- Example usage
main :: IO ()
main = do
  -- let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n"
  -- Open the file
  handle <- openFile "1.input" ReadMode
  -- Read the file line by line
  input <- hGetContents handle

  case parseGames input of
    Left err -> print err
    Right games ->
      let validGames = Maybe.mapMaybe processGame games
       in print (sum validGames)

  -- Close the file
  hClose handle
