{-#LANGUAGE OverloadedLists #-}
----------------------------------------------------------
--
-- Module      :  AI.AIFH.ClusterTest
-- Copyright   :
-- License     :  Apache
--
-- Maintainer  :  JP Moresmau <jp@moresmau.fr>
-- Stability   :  experimental
-- Portability :
--
-- | Clustering tests
--
-----------------------------------------------------------------------------

module AI.AIFH.ClusterTest (
    clusterTests
) where

import AI.AIFH.Cluster

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Set as DS
import qualified Data.Vector as V

-- | clustering tests
clusterTests :: TestTree
clusterTests = testGroup "Cluster Tests"
    [
      testCase "KMeans Random" $ doSimpleKMeansTest Random
    , testCase "KMeans Forgy" $ doSimpleKMeansTest Forgy
    ]

-- | Simple kmeans test. This may fail randomly, since the algorithm is random, sometimes we get funny results
doSimpleKMeansTest :: InitMode -> IO ()
doSimpleKMeansTest mode = do
  let opts = KMeansOptions 3 mode
      dats = [[0.1::Double,0.1],[10,10],[0.2,0.2],[-10,-10],[10,9.98],[-10,-9.8],[0.15,0.15],[0.14,0.14]]
  cls <- kMeans dats opts
  let expected = DS.fromList [DS.fromList [[0.1,0.1],[0.2,0.2],[0.15,0.15],[0.14,0.14]],DS.fromList [[10,10],[10,9.98]],DS.fromList [[-10,-10],[-10,-9.8]]]
      actual = DS.fromList $ V.toList $ V.map clElements cls
  expected @=? actual
