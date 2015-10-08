-- | This example reads a CSV file
module Main where

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
        -- read simply as a tuple
        let result = decode HasHeader bs :: Either String (Vector (Double,Double,Double,Double,String))
        case result of
          Left err  -> print $ "Error reading CSV File:" ++ err
          Right dat -> mapM_ print dat
