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

main :: IO ()
main = do
  print (testFn "Hello")

--- | testFn sample doctest
---
--- >>> testFn "hello"
-- Nothing
testFn :: String -> Maybe Int
testFn "" = Nothing
testFn s = trace ("sample trace" ++ show s) (Just 1)
