{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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
-- | Iris data set types and functions
--
-----------------------------------------------------------------------------

module AI.AIFH.DataSets.Iris where

import GHC.Generics
import Data.Csv
import Data.List
import Data.String
import Data.Ord
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import AI.AIFH.Normalize


-- | Raw Iris information
type Iris = GIris T.Text

-- | Normalized Iris information
type NormalizedIris = GIris (VU.Vector Double)

-- | Generic Iris type
data GIris a = GIris
  { irisSepalLength :: !Double
  , irisSepalWidth  :: !Double
  , irisPetalLength :: !Double
  , irisPetalWidth  :: !Double
  , irisClass       :: !a
  } deriving (Show,Read,Eq,Ord,Generic)

-- | Normalized Data Set
data NormalizedIrises = NormalizedIrises
  { nisIrises :: [NormalizedIris]
  , nisClassEncoding :: M.Map T.Text (VU.Vector Double)
  }

-- | Read Raw Iris from CSV
instance FromRecord Iris
-- | Write Raw Iris to CSV
instance ToRecord Iris

-- | CSV with field names
instance FromNamedRecord Iris where
    parseNamedRecord m = GIris <$> m .: "sepal_length"
                               <*> m .: "sepal_width"
                               <*> m .: "petal_length"
                               <*> m .: "petal_width"
                               <*> m .: "class"
instance ToNamedRecord Iris where
    toNamedRecord (GIris sl sw pl pw cl) = namedRecord [
        "sepal_length" .= sl, "sepal_width" .= sw, "petal_length" .= pl, "petal_width" .= pw, "class" .= cl]
instance DefaultOrdered Iris where
    headerOrder _ = header ["sepal_length","sepal_width","petal_length","petal_width","class"]

-- | Normalized Irises from CSV, with variable number of class columns
instance FromNamedRecord NormalizedIris where
    parseNamedRecord m = do
        let cls = map snd $ sortBy (comparing fst) $ filter (\(k,_)->BS.isPrefixOf "class-" k) $ HM.toList m
        classes <- mapM parseField cls
        GIris <$> m .: "sepal_length"
               <*> m .: "sepal_width"
               <*> m .: "petal_length"
               <*> m .: "petal_width"
               <*> pure (VU.fromList classes)

-- | Normalized Irises to CSV, with variable number of class columns
instance ToNamedRecord NormalizedIris where
    toNamedRecord (GIris sl sw pl pw cl) = namedRecord $ [
        "sepal_length" .= sl, "sepal_width" .= sw, "petal_length" .= pl, "petal_width" .= pw]
        ++ (map (\(idx,d)->fromString ("class-"++(show idx)) .= d) $ zip [0..] (VU.toList cl))

-- | Generate the Header for a normalized CSV
-- We cannot use the DefaultOrdered typeclass since we need the object to know how many columns to generate
normalizedIrisHeader :: NormalizedIrises -> Header
normalizedIrisHeader ni = header $ ["sepal_length","sepal_width","petal_length","petal_width"]
        ++ (map (\idx-> fromString ("class-"++(show (idx-1)))) [1..VU.length (head $ M.elems $ (nisClassEncoding ni))])


-- | Normalize the data set
normalizeIrises
    :: Foldable t
    => t Iris -> NormalizedIrises
normalizeIrises is = let
        (sepalLengths,sepalWidths,petalLengths,petalWidths,classes) = foldr toLists ([],[],[],[],[]) is
       -- equilateral encoding is from -1 to 1, we normalize to (0,1)
        (encodedClasses, encoding) = normEqui $ equilateralEncode classes
        nirises = zipWith5 GIris (normalizeRange (0,1) sepalLengths)
                            (normalizeRange (0,1) sepalWidths)
                            (normalizeRange (0,1) petalLengths)
                            (normalizeRange (0,1) petalWidths)
                            encodedClasses
        in NormalizedIrises nirises encoding
        where
            toLists (GIris sl sw pl pw c) (sls,sws,pls,pws,cs) = (sl:sls,sw:sws,pl:pls,pw:pws,c:cs)
            norm01 :: VU.Vector Double -> VU.Vector Double
            norm01 = normalizeRangeWithBounds (0,1) (-1,1)
            normEqui (cls,encoding)=(map norm01 cls,M.map norm01 encoding)
