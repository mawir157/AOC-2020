import AdventHelper

import Data.List

getSeat :: String -> String -> Integer
getSeat ones s = sum $ zipWith (*) b $ zipWith (^) (repeat 2) [0..(length s - 1)]
  where b = reverse $ map (\x -> if' (elem x ones) 0 1) s 

missing :: [Integer] -> Integer
missing [x] = x
missing (x:y:xs) = if' (succ x /= y) (succ x) (missing (y:xs))

main = do
  putStrLn "Day 5"
  f <- readFile "../input/input05.txt"
  let s = map (getSeat "FL") $ lines f

  printSoln 1 (maximum s)
  printSoln 2 (missing $ sort s)
