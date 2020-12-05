import AdventHelper

import Data.List

binSearch :: String -> String -> Integer
binSearch [c0,c1] s = sum $ zipWith (*) b $ zipWith (^) (repeat 2) [0..6]
  where b = reverse $ map (\x -> if' (x == c0) 0 1) s

getSeat :: String -> Integer
getSeat s = (8 * r) + c
  where r = binSearch "FB" (take 7 s)
        c = binSearch "LR" (drop 7 s)

missing :: [Integer] -> Integer
missing [x] = x
missing (x:y:xs) = if' (x + 1 /= y) (x+1) (missing (y:xs))

main = do
  putStrLn "Day 5"
  f <- readFile "../input/input05.txt"
  let s = map (getSeat) $ lines f

  printSoln 1 (maximum s)
  printSoln 2 (missing $ sort s)
