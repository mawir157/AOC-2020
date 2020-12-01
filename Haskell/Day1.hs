import AdventHelper

parseInput x = read(x) :: Integer

sumBy2Indices :: [Integer] -> (Int, Int) -> Integer
sumBy2Indices xs (v1,v2) = (xs!!v1 + xs!!v2)

sumBy3Indices :: [Integer] -> (Int, Int, Int) -> Integer
sumBy3Indices xs (v1,v2, v3) = (xs!!v1 + xs!!v2 + xs!!v3)

main = do
  putStrLn "Day 1"
  f <- readFile "../input/input01.txt"
  let s = map(parseInput) $ lines f
  let l = (length s)-1

  let pairs = [ (x,y) | x <- [0..l], y <- [0..l]]
  let sums = zipWithFn (sumBy2Indices s) pairs
  let ((v1, v2),_) = head $ dropWhile (\(x,p) -> p /= 2020) sums
  printSoln 1 ((s!!v1)*(s!!v2))

  let triples = [ (x,y,z) | x <- [0..l], y <- [0..l], z <- [0..l]]
  let sums3 = zipWithFn (sumBy3Indices s) triples
  let ((w1, w2, w3),_) = head $ dropWhile (\(x,p) -> p /= 2020) sums3
  printSoln 2 ((s!!w1)*(s!!w2)*(s!!w3))
