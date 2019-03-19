-- | Clusters the Iris data set into 3 clusters
module Main
    (
    main
    ) where

import AI.AIFH.Cluster
import AI.AIFH.DataSets.Iris

import Control.Monad

import System.Directory

import Data.Csv
import Data.Maybe
import Data.Tuple
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Map as DM
import qualified Data.Set as DS

-- | entry point
main :: IO ()
main = do
  NormalizedIrises irises clsMap <- normalize
  -- massage the data: unnormalize the class names, put all numeric data in a vector
  let clsMapRev = DM.fromList $ map swap $ DM.toList clsMap
      vectorized = map (\(GIris sl sw pl pw cls) -> (VU.fromList [sl,sw,pl,pw],fromJust $ DM.lookup cls clsMapRev)) irises
      dataToCls = DM.fromList vectorized
  -- kmeans
  km <- kMeans (DM.keys dataToCls) (KMeansOptions (DM.size clsMapRev) Random)
  -- print result
  forM_ km $ \c -> do
    print $ clCentroid c
    forM_ (DS.toList $ clElements c) $ \e -> do
      let cls = fromJust $ DM.lookup e dataToCls
      putStrLn ("\t" ++ (show $ VU.toList e) ++ " " ++ (show cls))

-- | normalize
normalize :: IO (NormalizedIrises)
normalize = do
   let f="data/iris.csv"
   ex <- doesFileExist f
   case ex of
     False -> error "Input file does not exist"
     _ -> do
        bs <- BS.readFile f
        -- read irises
        let result = decode HasHeader bs :: Either String (V.Vector Iris)
        case result of
          Left err  -> error $ "Error reading CSV File:" ++ err
          Right dat -> do
            let norm = normalizeIrises dat
            return norm
