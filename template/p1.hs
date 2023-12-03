#!/usr/bin/env runhaskell
import Data.Char qualified as Char
import Data.Maybe qualified as Maybe
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
