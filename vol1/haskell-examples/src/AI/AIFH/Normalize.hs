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

    , equilateralEncode
    , equilateralDecode
    )where

import AI.AIFH.Utils

import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import qualified Data.Map as M

-- | Normalize numbers, given a normalization range
normalizeRange :: (Fractional a, Ord a, Functor f,Foldable f) => (a,a) -> f a -> f a
normalizeRange normalizedBounds vals
    | null vals = fmap id vals
    | otherwise =
        let dataLow = minimum vals
            dataHigh = maximum vals
        in normalizeRangeWithBounds normalizedBounds (dataLow,dataHigh) vals

-- | Normalize numbers, given a normalization range and a data range
normalizeRangeWithBounds :: (Fractional a,Ord a,Functor f) => (a,a) -> (a,a) -> f a -> f a
normalizeRangeWithBounds (normalizedLow,normalizedHigh) (dataLow,dataHigh) vals
    | dataLow == dataHigh =  ((normalizedHigh + normalizedLow) / 2) <$ vals
    | otherwise =
        fmap scale vals
        where scale x = ((x - dataLow)
                            / (dataHigh - dataLow))
                            * (normalizedHigh - normalizedLow) + normalizedLow

-- | Denormalize numbers, given a normalization range and a data range
denormalizeRangeWithBounds :: (Fractional a,Ord a,Functor f) => (a,a) -> (a,a) -> f a -> f a
denormalizeRangeWithBounds (normalizedLow,normalizedHigh) (dataLow,dataHigh) =
    fmap scale
    where scale x = ((dataLow - dataHigh) * x - normalizedHigh
                    * dataLow + dataHigh * normalizedLow)
                    / (normalizedLow - normalizedHigh)

-- | Encode a list of items using equilateral encoding
--   return the encoded items  + the map used to encode
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
