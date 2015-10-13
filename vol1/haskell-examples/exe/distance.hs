module Main where

import AI.AIFH.Utils

main :: IO()
main = do
    let pos1 = [1.0, 2.0, 3.0]
        pos2 = [4.0, 5.0, 6.0]
        pos3 = [7.0, 8.0, 9.0]
    putStrLn "Euclidean Distance"
    putStrLn("pos1->pos2: " ++ show(euclidean pos1 pos2))
    putStrLn("pos2->pos3: " ++ show(euclidean pos2 pos3))
    putStrLn("pos3->pos1: " ++ show(euclidean pos3 pos1))
    putStrLn ""
    putStrLn("Manhattan (city block) Distance")
    putStrLn("pos1->pos2: " ++ show(manhattan pos1 pos2))
    putStrLn("pos2->pos3: " ++ show(manhattan pos2 pos3))
    putStrLn("pos3->pos1: " ++ show(manhattan pos3 pos1))
    putStrLn ""
    putStrLn("Chebyshev Distance")
    putStrLn("pos1->pos2: " ++ show(chebyshev pos1 pos2))
    putStrLn("pos2->pos3: " ++ show(chebyshev pos2 pos3))
    putStrLn("pos3->pos1: " ++ show(chebyshev pos3 pos1))
