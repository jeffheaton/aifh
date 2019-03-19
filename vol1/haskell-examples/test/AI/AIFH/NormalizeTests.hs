{-#LANGUAGE OverloadedLists #-}
----------------------------------------------------------
--
-- Module      :  AI.AIFH.NormalizeTests
-- Copyright   :
-- License     :  Apache
--
-- Maintainer  :  JP Moresmau <jp@moresmau.fr>
-- Stability   :  experimental
-- Portability :
--
-- | Normalization tests
--
-----------------------------------------------------------------------------

module AI.AIFH.NormalizeTests (
    normalizeTests
) where

import AI.AIFH.Normalize
import AI.AIFH.Utils

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

--import Debug.Trace

import qualified Data.Vector.Unboxed as VU

normalizeTests :: TestTree
normalizeTests = testGroup "Normalize Tests"
    [ testGroup "Normalize Range"
        [ testCase "Book example" $ do
                [-0.54] @=? map (roundTo 2) (normalizeRangeWithBounds (-1,1) (100,4000) [1000])
                [-1,1,-0.54] @=? map (roundTo 2) (normalizeRange (-1,1) [100,4000,1000])
                [100,4000,997] @=? map (roundTo 2) (denormalizeRangeWithBounds (-1,1) (100,4000) [-1,1,-0.54])
        , testProperty "Normalize/Denormalize round trip" propNormalizeRoundTrip
        ]
    , testGroup "One-of-N Encoding"
        [ testCase "Book examples" $ do
            testOneOfNExplicit ['a','c','b'] [[1,-1,-1],[-1,1,-1],[-1,-1,1]]
        , testProperty "One of N round trip" propOneOfNRoundTrip
        ]
    , testGroup "Equilateral Encoding"
        [ testCase "Book examples" $ do
            testEquilateralExplicit ['a','b'] [[-1],[1]]
            testEquilateralExplicit ['a','b','c'] [[-0.866,-0.5],[0.866,-0.5],[0,1]]
            testEquilateralExplicit ['a','b','c','d'] [[-0.816,-0.471,-0.333],[0.816,-0.471,-0.333],[0.0,0.943,-0.333],[0.0,0.0,1.0]]
        , testProperty "Equilateral round trip" propEquilateralRoundTrip
        ]
    ]

-- | Test we can round trip betwee normalization and denormalization
propNormalizeRoundTrip :: NonEmptyList Double -> Bool
propNormalizeRoundTrip valsRaw =
    let vals = map (roundTo 2) $ getNonEmpty valsRaw
        dataLow = minimum vals
        dataHigh = maximum vals
        normBounds = (-1,1)
        dataBounds= (dataLow,dataHigh)
        res = map (roundTo 2) (denormalizeRangeWithBounds normBounds dataBounds (normalizeRange normBounds vals))
    in vals == res

testOneOfNExplicit :: [Char] -> [VU.Vector Double] -> IO()
testOneOfNExplicit vals output = do
    let (v,m) = oneOfNEncode vals
    map (VU.map (roundTo 3)) v @?= output
    vals @=? oneOfNDecode m v

propOneOfNRoundTrip :: NonEmptyList Char -> Bool
propOneOfNRoundTrip valsRaw =
    let vals = getNonEmpty valsRaw
        (v,m) = oneOfNEncode vals
    in vals == oneOfNDecode m v

testEquilateralExplicit :: [Char] -> [VU.Vector Double] -> IO()
testEquilateralExplicit vals output = do
    let (v,m) = equilateralEncode vals
    map (VU.map (roundTo 3)) v @?= output
    vals @=? equilateralDecode m v

propEquilateralRoundTrip :: NonEmptyList Char -> Bool
propEquilateralRoundTrip valsRaw =
    let vals = getNonEmpty valsRaw
        (v,m) = equilateralEncode vals
    in vals == equilateralDecode m v
