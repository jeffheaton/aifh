{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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
-- | Normalization functions
--
-----------------------------------------------------------------------------

module AI.AIFH.Normalize
    ( normalizeRange
    , normalizeRangeWithBounds
    , denormalizeRangeWithBounds

    , oneOfNEncode
    , oneOfNDecode
    , equilateralEncode
    , equilateralDecode
    )where

import AI.AIFH.Utils

import Data.List
import Data.Maybe
import Data.Ord
import Data.Tuple
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import qualified Data.Map as M

-- | Normalize numbers, given a normalization range
normalizeRange :: (Fractional a, Ord a, Normalizable f a,Foldable f) => (a,a) -> f a -> f a
normalizeRange normalizedBounds vals
    | null vals = vals
    | otherwise =
        let dataLow = minimum vals
            dataHigh = maximum vals
        in normalizeRangeWithBounds normalizedBounds (dataLow,dataHigh) vals

-- | A typeclass to allow us to normalize over various structures for flexibility
class (Fractional a,Ord a) => Normalizable n a where
    -- | Normalize numbers, given a normalization range and a data range
    normalizeRangeWithBounds :: (a,a) -> (a,a) -> n a -> n a
    -- | Denormalize numbers, given a normalization range and a data range
    denormalizeRangeWithBounds :: (a,a) -> (a,a) -> n a -> n a

-- | normalize over a Functor
instance {-# OVERLAPPABLE #-} (Fractional a,Ord a,Functor f) => Normalizable f a where
    normalizeRangeWithBounds (normalizedLow,normalizedHigh) (dataLow,dataHigh) vals
        | dataLow == dataHigh =  ((normalizedHigh + normalizedLow) / 2) <$ vals
        | otherwise =
            fmap scale vals
            where scale x = ((x - dataLow)
                                / (dataHigh - dataLow))
                                * (normalizedHigh - normalizedLow) + normalizedLow
    denormalizeRangeWithBounds (normalizedLow,normalizedHigh) (dataLow,dataHigh) =
        fmap scale
        where scale x = ((dataLow - dataHigh) * x - normalizedHigh
                        * dataLow + dataHigh * normalizedLow)
                        / (normalizedLow - normalizedHigh)

-- | Normalize over an unboxed vector
instance {-# OVERLAPPING #-} (Fractional a,Ord a,VU.Unbox a) => Normalizable VU.Vector a where
    normalizeRangeWithBounds (normalizedLow,normalizedHigh) (dataLow,dataHigh) vals
        | dataLow == dataHigh =  VU.replicate (VU.length vals) ((normalizedHigh + normalizedLow) / 2)
        | otherwise =
            VU.map scale vals
            where scale x = ((x - dataLow)
                                / (dataHigh - dataLow))
                                * (normalizedHigh - normalizedLow) + normalizedLow
    denormalizeRangeWithBounds (normalizedLow,normalizedHigh) (dataLow,dataHigh) =
        VU.map scale
        where scale x = ((dataLow - dataHigh) * x - normalizedHigh
                        * dataLow + dataHigh * normalizedLow)
                        / (normalizedLow - normalizedHigh)

-- | Encode a list of items using one of n encoding
-- return the encoded items + the map used to encode
oneOfNEncode :: (Ord a) => [a] -> ([VU.Vector Double],M.Map a Int)
oneOfNEncode vals =
    let
        allVals = ordNub vals
        encoding = M.fromList $ zip allVals [0..]
        emptyvu  = VU.replicate (length allVals) (-1)
        result = map (setIndex encoding emptyvu) vals
     in (result, encoding)
     where
       setIndex encoding emptyvu v =
        let ix=fromJust $ M.lookup v encoding
        in emptyvu VU.// [(ix,1)]

-- | Decode a list of items using one of n encoding
oneOfNDecode :: (Ord a) => M.Map a Int -> [VU.Vector Double] -> [a]
oneOfNDecode encoding = map oneofNDecoding
    where
        rs = M.fromList $ map swap $ M.assocs encoding
        oneofNDecoding v =
            let ix = VU.maxIndex v
            in fromJust $ M.lookup ix rs

-- | Encode a list of items using equilateral encoding
--   return the encoded items + the map used to encode
equilateralEncode :: (Ord a) => [a] -> ([VU.Vector Double],M.Map a (VU.Vector Double))
equilateralEncode vals =
    let
        allVals = ordNub vals
        encoding = equilateralEncoding allVals
        result = map (\v->fromJust $ M.lookup v encoding) vals
    in (result,encoding)

-- | Decode a list of items using equilateral encoding
equilateralDecode :: (Ord a) => M.Map a (VU.Vector Double) -> [VU.Vector Double] -> [a]
equilateralDecode m  = map (equilateralDecoding m)

-- | Generate equilateral encoding information: a map from each item to the list of numbers representing it
equilateralEncoding :: (Ord a) => [a] -> M.Map a (VU.Vector Double)
equilateralEncoding vals =
    let n = length vals
        z1 = VU.replicate (n-2) 0
        z2 = VU.replicate (n-1) 0
        v0 = V.cons (VU.cons (-1) z1) $ V.cons (VU.cons 1 z1) $ V.replicate (n-2) z2
        v1 = foldl' pass v0 [2..n-1]
    in foldr (\(a,x) m-> M.insert a (v1 V.! x) m) M.empty $ zip vals [0..]
    where
        r :: Double -> Double
        r k = -1 / k
        f :: Double -> Double
        f k = sqrt (k*k -1) / k
        pass :: V.Vector (VU.Vector Double) -> Int -> V.Vector (VU.Vector Double)
        pass m1 ik =
            let k = fromIntegral ik
                f1 = f k
                a0 = map (\i->(i,map (\j->(j,f1)) [0..ik-2])) [0..ik-1]
                m2 = V.accum (\a b -> VU.accum (*) a b) m1 a0
                r1 = r k
                a1 = map (\x->(x,[(ik-1,r1)])) [0..ik-1] ++ [(ik,[(ik-1,1)])]
                m3 = V.accum (\a b -> a VU.// b) m2 a1
            in m3

-- | Decode an item using equilateral encoding
equilateralDecoding :: M.Map a (VU.Vector Double) -> VU.Vector Double -> a
equilateralDecoding m os =
    let rs = M.assocs m
        es = map (\(ix,vs)->(ix,euclidianV vs os)) rs
    in fst $ minimumBy (comparing snd) es
