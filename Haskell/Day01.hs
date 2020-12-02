import AdventHelper

sum_and_prod :: Integer -> [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
sum_and_prod _ [x] _ = []
sum_and_prod t (x:xs) ys = q ++ (sum_and_prod t xs ys)
  where q = map (\(s',p') -> ((fst x)+s', (snd x)*p')) ys

main = do
  putStrLn "Day 1"
  f <- readFile "../input/input01.txt"
  let s = map(read) $ lines f :: [Integer]
  let p0 = zip s s
  let t = 2020

  let p1 = sum_and_prod t p0 p0
  printSoln 1 (snd $ head (dropWhile (\l -> fst l /= t) p1))
  let p2 = sum_and_prod t (filter (\(s,_) -> s < t) p1) p0
  printSoln 2 (snd $ head (dropWhile (\l -> fst l /= t) p2))
