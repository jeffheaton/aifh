-- | Entry point for tests
module Main where

import Test.Tasty

import AI.AIFH.NormalizeTests

main :: IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [normalizeTests]
