-----------------------------------------------------------------------------
--
-- Module      :  AI.AIFH.NormalizeTests
-- Copyright   :
-- License     :  Apache
--
-- Maintainer  :  JP Moresmau <jp@moresmau.fr>
-- Stability   :  experimental
-- Portability :
--
-- |
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

import Debug.Trace

normalizeTests :: TestTree
normalizeTests = testGroup "Normalize Tests" [
    testGroup "NormalizeRange"
        [ testCase "Book example" $ do
                [-0.54] @=? map (roundTo 2) (normalizeRangeWithBounds (-1,1) (100,4000) [1000])
                [-1,1,-0.54] @=? map (roundTo 2) (normalizeRange (-1,1) [100,4000,1000])
                [100,4000,997] @=? map (roundTo 2) (denormalizeRangeWithBounds (-1,1) (100,4000) [-1,1,-0.54])
        , testProperty "Normalize/Denormalize round trip" propNormalizeRoundTrip
        ]
    ]

propNormalizeRoundTrip :: NonEmptyList Double -> Bool
propNormalizeRoundTrip valsRaw =
    let vals = map (roundTo 2) $ getNonEmpty valsRaw
        dataLow = minimum vals
        dataHigh = maximum vals
        normBounds = (-1,1)
        dataBounds= (dataLow,dataHigh)
        res = map (roundTo 2) (denormalizeRangeWithBounds normBounds dataBounds (normalizeRange normBounds vals))
    in vals == res
