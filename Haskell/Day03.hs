import AdventHelper

import Data.List

tbg :: Int -> [String] -> Int -> (Int, Int) -> Int
tbg _ [] _ _ = 0
tbg w (s:ss) y (dx, dy) = tree + (tbg w ss' y' (dx,dy))
  where tree = if' (s!!y == '#') 1 0
        y' = (y + dy) `mod` w
        ss' = drop (dx-1) ss

main = do
  putStrLn "Day 3"
  f <- readFile "../input/input03.txt"
  let s = lines f
  let w = length $ head s

  printSoln 1 (tbg w s 0 (1,3))
  printSoln 2 (product $ map (tbg w s 0) [(1,1),(1,3),(1,5),(1,7),(2,1)])
