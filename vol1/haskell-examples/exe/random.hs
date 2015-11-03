{-# LANGUAGE ScopedTypeVariables #-}
-- | Chapter 4: random numbers
module Main where

import AI.AIFH.Utils

import Control.Monad
import Data.List
import System.Environment

import System.Random.MWC
import System.Random.MWC.Distributions

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

-- | Entry point
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["uniform"] -> generateHistogram Uniform
        ["normal"]  -> generateHistogram Normal
        _ -> do
            n<-getProgName
            putStrLn $ "usage "++n++" uniform|normal"

-- | Type of distribution
data Distribution = Uniform | Normal
    deriving (Show,Read,Eq,Ord,Bounded,Enum)

-- | Generate the frequency histogram to a png file
generateHistogram :: Distribution -> IO ()
generateHistogram d = do
    -- random values
    vals::[Double] <- withSystemRandom . asGenST $ \gen -> replicateM 10000 (method d gen)
    -- frequency: sort, group and link value and length of group
    let hist::[(Double,[Int])] = map (\xs->(head xs,[length xs])) $ group $ sort $ map (roundTo 2) vals
    -- generate bar chart
    toFile def (show d ++ ".png") $ do
        layout_title .= show d ++ " Distribution"
        plot $ fmap plotBars (bars ["Frequency"] hist)
    where
        method Uniform = uniform
        method Normal = standard
