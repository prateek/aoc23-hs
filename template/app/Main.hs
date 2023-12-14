{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Advent qualified
import Data.Char qualified as Char
import Data.Either qualified
import Data.Function ((&))
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Maybe qualified as M
import Data.Ord qualified as O
import Data.Set qualified as S
import Data.Text qualified
import Debug.Trace (trace)
import System.Environment (lookupEnv)
import System.IO ()

main :: IO ()
main = do
  sessionKey <- lookupEnv "AOC_SESSION_KEY"
  let opts = mkAoCOpts 2023 (M.fromJust sessionKey)
  inputTxt <- Advent.runAoC opts $ Advent.AoCInput (Advent.mkDay_ 13)
  case inputTxt of
    Left _ -> fail "unable to get data"
    Right txt -> do
      print (Data.Text.unpack txt)

testInput = "test.input"

mkAoCOpts year token = Advent.defaultAoCOpts (Advent.AoCUserAgent repo contact) year token
  where
    repo = Data.Text.pack "https://github.com/prateek/aoc23-hs"
    contact = Data.Text.pack "https://github.com/prateek"
