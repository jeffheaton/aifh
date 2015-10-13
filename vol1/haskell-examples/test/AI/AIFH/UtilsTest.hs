-----------------------------------------------------------------------------
--
-- Module      :  AI.AIFH.UtilsTest
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

module AI.AIFH.UtilsTest (
    utilsTests
) where


import AI.AIFH.Utils

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

utilsTests :: TestTree
utilsTests = testGroup "Utils Tests"
    [ testGroup "Distance Tests"
         [ testCase "Examples" $ do
            let pos1 = [1.0, 2.0, 3.0]
                pos2 = [4.0, 5.0, 6.0]
                pos3 = [7.0, 8.0, 9.0]
            5.196 @=? roundTo 3 (euclidean pos1 pos2)
            5.196 @=? roundTo 3 (euclidean pos2 pos3)
            10.392 @=? roundTo 3 (euclidean pos3 pos1)
            9 @=? manhattan pos1 pos2
            9 @=? manhattan pos2 pos3
            18 @=? manhattan pos3 pos1
            3 @=? chebyshev pos1 pos2
            3 @=? chebyshev pos2 pos3
            6 @=? chebyshev pos3 pos1
         ]
    ]
