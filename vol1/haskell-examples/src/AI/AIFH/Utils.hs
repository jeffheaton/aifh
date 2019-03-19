{-# LANGUAGE RankNTypes #-}
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

import Data.List
import Data.Ord

import qualified Data.Set as S

import qualified Data.Vector.Unboxed as VU

-- | Round a number to n decimal digits
roundTo :: (Fractional a, RealFrac r) =>
                 Int -> r -> a
roundTo n f=  fromInteger (round $ f * (10^n)) / (10.0^^n)

type DistanceV a = (VU.Unbox a,Fractional a) => VU.Vector a -> VU.Vector a -> a

-- | Euclidean distance
euclidean :: (Floating a) => [a] -> [a] -> a
euclidean v1 v2 = sqrt $ sum $ zipWith (\a b -> (a-b)**2) v1 v2

-- | Euclidean distance on unboxed vectors, for performance
euclideanV :: (Floating a) => DistanceV a
euclideanV v1 v2 = sqrt $ VU.sum $ VU.zipWith (\a b -> (a-b)**2) v1 v2

-- | Manhattan distance
manhattan :: (Floating a) => [a] -> [a] -> a
manhattan v1 v2 = sum $ zipWith (\a b -> abs (a-b)) v1 v2

-- | Manhattan distance on unboxed vectors, for performance
manhattanV :: DistanceV a
manhattanV v1 v2 = VU.sum $ VU.zipWith (\a b -> abs(a-b)) v1 v2

-- | Chebyshev distance
chebyshev :: (Floating a,Ord a) => [a] -> [a] -> a
chebyshev v1 v2 = maximum $ zipWith (\a b -> abs (a-b)) v1 v2

-- | Chebyshev distance on unboxed vectors, for performance
chebyshevV :: (Ord a) => DistanceV a
chebyshevV v1 v2 = VU.maximum $ VU.zipWith (\a b -> abs(a-b)) v1 v2

-- | Find the nearest vector to a given point
nearest :: (VU.Unbox a,Fractional a,Ord a) => DistanceV a -> [VU.Vector a] -> VU.Vector a -> VU.Vector a
nearest disV vects pos = fst $ minimumBy (comparing snd) $ map (dist pos) vects
    where dist po vect = (vect,disV vect po)

-- | efficient removal of duplicates
ordNub :: (Ord a) => [a] -> [a]
ordNub = go S.empty
     where
       go _ []     = []
       go s (x:xs) = if x `S.member` s then go s xs
                                     else x : go (S.insert x s) xs
