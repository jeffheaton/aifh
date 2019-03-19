{-# LANGUAGE PatternGuards #-}
-- | Chapter 4: approximation of pi
module Main where

import System.Environment
import System.Random.MWC


-- | entry point. Argument is number of random points to try
main :: IO()
main = do
    args <- getArgs
    case parseTries args of
        Nothing -> do
            n<-getProgName
            putStrLn $ "usage "++n++" <iterations>"
        Just tries -> do
            successes <- withSystemRandom . asGenIO $ \gen -> do
                    go gen tries 0
            print $ 4 * (fromIntegral successes) / (fromIntegral tries)
    where
        go _ 0 cnt = return cnt
        go gen t cnt = do
            x <- uniform gen
            y <- uniform gen
            go gen (t-1) $! (isInside cnt (x,y)) -- make count strict to restrict memory

-- | If the point is inside the circle, increment the count
isInside :: Int -> (Double,Double) -> Int
isInside cnt (x,y)
    | x*x + y*y<=1 = cnt+1
    | otherwise = cnt

-- | Parse number of tries from arguments
parseTries :: [String] -> Maybe Int
parseTries [] = Nothing
parseTries (x:_)
    | [(i,[])] <- reads x = Just i
    | otherwise = Nothing
