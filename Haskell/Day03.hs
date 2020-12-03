import AdventHelper

import Data.List

parseInput :: String -> [Int]
parseInput ss =  elemIndices '#' ss

tbg :: Int -> [[Int]] -> (Int, Int) -> (Int, Int) -> Int
tbg _ [] _ _ = 0
tbg w (s:ss) (x,y) (dx, dy) = tree + (tbg w ss' (x, y') (dx,dy))
  where tree = if' (elem y s) 1 0
        y' = (y + dy) `mod` w
        ss' = drop (dx-1) ss

main = do
  putStrLn "Day 3"
  f <- readFile "../input/input03.txt"
  let s = map(parseInput) $ lines f
  let w = length $ head $ lines f

  printSoln 1 (tbg w s (0,0) (1,3))
  printSoln 2 (product $ map (tbg w s (0,0)) [(1,1),(1,3),(1,5),(1,7),(2,1)])
