import AdventHelper

parseInput :: String -> ((Int, Int), Char, String)
parseInput s = ((lo, hi), head (t!!2), t!!3)
  where t = splitOnAnyOf [": ", "-", " "] s
        lo = read (t!!0) :: Int
        hi = read (t!!1) :: Int

isValid :: ((Int, Int), Char, String) -> Bool
isValid ((lo, hi), c, pw) = (ct >= lo) && (ct <= hi)
  where ct = length $ filter (c ==) pw

isValid2 :: ((Int, Int), Char, String) -> Bool
isValid2 ((lo, hi), c, pw) = ((pw!!(lo-1)) == c) /= ((pw!!(hi-1)) == c)

main = do
  putStrLn "Day 2"
  f <- readFile "../input/input02.txt"
  let s = map(parseInput) $ lines f
  printSoln 1 (length $ filter (isValid) s)
  printSoln 1 (length $ filter (isValid2) s)
