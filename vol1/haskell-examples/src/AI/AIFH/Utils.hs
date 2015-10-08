-----------------------------------------------------------------------------
--
-- Module      :  AI.AIFH.Utils
-- Copyright   :
-- License     :  Apache
--
-- Maintainer  :  JP Moresmau <jp@moresmau.fr>
-- Stability   :  experimental
-- Portability :
--
-- | Utility functions
--
-----------------------------------------------------------------------------

module AI.AIFH.Utils where

import qualified Data.Set as S

import qualified Data.Vector.Unboxed as VU

-- | Round a number to n decimal digits
roundTo :: (Fractional a, Integral b, RealFrac r) =>
                 b -> r -> a
roundTo n f=  fromInteger (round $ f * (10^n)) / (10.0^^n)

-- | Euclidian distance
euclidian :: (Floating a) => [a] -> [a] -> a
euclidian v1 v2 = sqrt $ sum $ zipWith (\a b -> (a-b)**2) v1 v2

-- | Euclidian distance on unboxed vectors, for performance
euclidianV :: VU.Vector Double -> VU.Vector Double -> Double
euclidianV v1 v2 = sqrt $ VU.sum $ VU.zipWith (\a b -> (a-b)**2) v1 v2

-- | efficient removal of duplicates
ordNub :: (Ord a) => [a] -> [a]
ordNub = go S.empty
     where
       go _ []     = []
       go s (x:xs) = if x `S.member` s then go s xs
                                     else x : go (S.insert x s) xs
