-----------------------------------------------------------------------------
--
-- Module      :  AI.AIFH.Cluster
-- Copyright   :
-- License     :  Apache
--
-- Maintainer  :  JP Moresmau <jp@moresmau.fr>
-- Stability   :  experimental
-- Portability :
--
-- | Clustering functions
--
-----------------------------------------------------------------------------

module AI.AIFH.Cluster
 ( KMeansOptions (..)
 , InitMode (..)
 , Cluster (..)
 , kMeans
 ) where

import AI.AIFH.Utils

import Control.Arrow

import Data.List
import Data.Ord

import System.Random.MWC
import System.Random.MWC.Distributions

import qualified Data.Set as DS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V


-- | Initialization mode
data InitMode = Random | Forgy
  deriving (Show,Read,Eq,Ord,Bounded,Enum)

-- | Options for KMeans algorithm
data KMeansOptions = KMeansOptions
  { kmoClusterNb :: Int -- ^ Number of requested clusters
  , kmoInitMode  :: InitMode
  } deriving (Show,Read,Eq,Ord)

-- | A Cluster is a centroid and elements
data Cluster a = Cluster
  { clCentroid :: VU.Vector a
  , clElements :: DS.Set (VU.Vector a)
  } deriving (Show,Read,Eq,Ord)

-- | KMeans algorithm
kMeans :: (Ord a,VU.Unbox a,Floating a,Show a,Variate a) => [VU.Vector a] -> KMeansOptions -> IO (V.Vector (Cluster a))
kMeans [] _ = error "no elements given to kMeans"
kMeans els opts = do
  let sz = VU.length $ head els
  start <- kMeansInit sz els opts
  kMeansStep sz start

-- | Initialize KMeans clusters
kMeansInit :: (Ord a,VU.Unbox a,Fractional a,Show a,Variate a) => Int -> [VU.Vector a] -> KMeansOptions -> IO (V.Vector (Cluster a))
kMeansInit sz els opts
  | kmoClusterNb opts > length els = error $ "not enough clusters ("++show (kmoClusterNb opts)++") for the number of elements ("++show(length els)++")"
  | otherwise = do
     -- shuffle the list of observations randomly
     shuffled <- withSystemRandom . asGenIO $ \gen ->
                  uniformShuffle (V.fromList els) gen
     case kmoInitMode opts of
      Random -> do
        -- create empty clusters then assign each observation in turn to each
        let cls = replicate (kmoClusterNb opts) (Cluster undefined DS.empty)
            filled = assign [] cls (V.toList shuffled)
        updated <- mapM (kMeansCentroid sz) filled
        return $ V.fromList updated
      Forgy -> do
        -- get the first observations as the centroids, then assign the others
        let (ctrs,others) = splitAt (kmoClusterNb opts) (V.toList shuffled)
            cls = map (\c->Cluster c (DS.singleton c)) ctrs
            filled = assign [] cls others
        return $ V.fromList filled
   where
      assign a b [] = a ++ b -- nothing to do
      assign a [] orphanEls = assign [] a orphanEls -- assigned to each cluster, start again
      assign a (cls:rs) (e:orphanEls) = assign ((cls{clElements=DS.insert e (clElements cls)}):a) rs orphanEls -- assign

-- | Run a step of KMeans clustering (assignment and recalculation of centroids) recursively until no changes are done
kMeansStep :: (Ord a,VU.Unbox a,Floating a,Variate a) => Int -> V.Vector (Cluster a) -> IO (V.Vector (Cluster a))
kMeansStep sz cls = do
    let (changed,stable) = kMeansAssign cls
    if V.null changed
        then return stable
        else do
          newGen <- V.mapM (kMeansCentroid sz) changed
          kMeansStep sz  (newGen V.++ stable)

-- | assign elements to clusters
-- return the list of changed clusters and the list of unchanged clusters
kMeansAssign :: (Ord a,VU.Unbox a,Floating a) => V.Vector (Cluster a) -> (V.Vector (Cluster a),V.Vector (Cluster a))
kMeansAssign cls =
  let origM = V.map (\c -> (c,False) ) cls
      ctrs  = V.imap (,) cls
      newM  = V.foldr (move ctrs) origM ctrs
  in V.map fst *** V.map fst $ V.partition snd newM
  where
    move ctrs (ix,Cluster _ elts) m = DS.foldr (move1 ctrs ix) m elts
    move1 ctrs ix elt m =
        let should = shouldMove ctrs ix elt
        in case should of
            Nothing   -> m
            Just near ->
              let (Cluster c1 elts1,_) = m V.! ix
                  c1' = Cluster c1 (DS.delete elt elts1)
                  (Cluster c2 elts2,_) = m V.! near
                  c2' = Cluster c2 (DS.insert elt elts2)
              in m V.// [(near,(c2',True)),(ix,(c1',True))]
    shouldMove ctrs ix elt =
        let near = nearestC ctrs elt
        in if near == ix
          then Nothing
          else Just near
    nearestC ctrs elt = fst $ minimumBy (comparing snd) $ V.map (dist elt) ctrs
    dist po ctr = (fst ctr,euclideanV (clCentroid $ snd ctr) po)

-- | Recalculate the centroid of a cluster from its elements position
kMeansCentroid :: (VU.Unbox a,Fractional a,Variate a) => Int -> Cluster a -> IO (Cluster a)
kMeansCentroid sz (Cluster _ elts)
  | DS.null elts = do -- no elements, generate a new random cluster
      rnd <- withSystemRandom . asGenIO $ \gen -> uniformVector gen sz
      return $ Cluster (VU.fromList $ V.toList rnd) elts
  | otherwise = do
    -- calculate mean
    let dsz = fromIntegral $ DS.size elts
    return $ Cluster (VU.map (/ dsz) $ foldr sumV zeroV elts) elts
    where
      zeroV = VU.replicate sz 0
      sumV = VU.zipWith (+)
