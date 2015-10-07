{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
--
-- Module      :  AI.AIFH.DataSets.Iris
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

module AI.AIFH.DataSets.Iris where

import GHC.Generics
import Data.Csv
import Data.List
import qualified Data.Text as T

import AI.AIFH.Normalize

data Iris = Iris
  { irisSepalLength :: !Double
  , irisSepalWidth  :: !Double
  , irisPetalLength :: !Double
  , irisPetalWidth  :: !Double
  , irisClass       :: !T.Text
  } deriving (Show,Read,Eq,Ord,Generic)



instance FromRecord Iris
instance ToRecord Iris

normalizeIrises
    :: Foldable t
    => t Iris -> [Iris]
normalizeIrises is = let
        (sepalLengths,sepalWidths,petalLengths,petalWidths,classes) = foldr toLists ([],[],[],[],[]) is
        in zipWith5 Iris (normalizeRange (0,1) sepalLengths)
                            (normalizeRange (0,1) sepalWidths)
                            (normalizeRange (0,1) petalLengths)
                            (normalizeRange (0,1) petalWidths)
                            classes
        where
            toLists (Iris sl sw pl pw c) (sls,sws,pls,pws,cs) = (sl:sls,sw:sws,pl:pls,pw:pws,c:cs)
