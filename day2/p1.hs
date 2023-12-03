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

-- Function to validate a Round
validateRound :: Round -> Bool
validateRound = all validateItem
  where
    validateItem (Item count color) = count <= Maybe.fromJust (lookup color validCounts)
    validCounts = [(Blue, 14), (Green, 13), (Red, 12)]

-- Function to process each game
processGame :: Game -> Maybe Int
processGame (Game id rounds)
  | and validParts = Just id
  | otherwise = Nothing
  where
    validParts = map validateRound rounds

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
