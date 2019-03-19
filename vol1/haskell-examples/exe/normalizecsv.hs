-- | This example reads a CSV file, normalizes the data set and writes the normalized data to a new file
module Main where

import AI.AIFH.DataSets.Iris

import System.Directory

import Data.Csv
import Data.ByteString.Lazy as BS
import Data.Vector (Vector)

main :: IO()
main = do
   let f="data/iris.csv"
   ex <- doesFileExist f
   case ex of
     False -> print "Input file does not exist"
     _ -> do
        bs <- BS.readFile f
        -- read irises
        let result = decode HasHeader bs :: Either String (Vector Iris)
        case result of
          Left err  -> print $ "Error reading CSV File:" ++ err
          Right dat -> do
            let norm = normalizeIrises dat
                outf = "normalized.csv"
                outbs = encodeByName (normalizedIrisHeader norm) $ nisIrises norm
            BS.writeFile outf outbs
