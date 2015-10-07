-----------------------------------------------------------------------------
--
-- Module      :  AI.AIFH.Normalize
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

module AI.AIFH.Normalize where

import Data.Functor

normalizeRange :: (Fractional a, Ord a, Functor f,Foldable f) => (a,a) -> f a -> f a
normalizeRange normalizedBounds vals
    | null vals = fmap id vals
    | otherwise =
        let dataLow = minimum vals
            dataHigh = maximum vals
        in normalizeRangeWithBounds normalizedBounds (dataLow,dataHigh) vals

normalizeRangeWithBounds :: (Fractional a,Ord a,Functor f) => (a,a) -> (a,a) -> f a -> f a
normalizeRangeWithBounds (normalizedLow,normalizedHigh) (dataLow,dataHigh) vals
    | dataLow == dataHigh =  ((normalizedHigh + normalizedLow) / 2) <$ vals
    | otherwise =
        fmap scale vals
        where scale x = ((x - dataLow)
                            / (dataHigh - dataLow))
                            * (normalizedHigh - normalizedLow) + normalizedLow

denormalizeRangeWithBounds :: (Fractional a,Ord a,Functor f) => (a,a) -> (a,a) -> f a -> f a
denormalizeRangeWithBounds (normalizedLow,normalizedHigh) (dataLow,dataHigh) =
    fmap scale
    where scale x = ((dataLow - dataHigh) * x - normalizedHigh
                    * dataLow + dataHigh * normalizedLow)
                    / (normalizedLow - normalizedHigh)
