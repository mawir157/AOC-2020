import AdventHelper

import Data.List
import Data.List.Split

parseRange :: String -> [Integer]
parseRange ss = ranges
  where parts = splitOn " or " $ drop 2 $ dropWhile (/= ':') ss
        pairs = map (map read . splitOn "-") parts  :: [[Integer]]
        ranges = concatMap (\[x, y] -> [x..y]) pairs

invalidFields :: [[Integer]] -> [Integer] -> [Integer]
invalidFields ranges ticket = foldl' (\\) ticket ranges

isInvalid :: [[Integer]] -> [Integer] -> Bool
isInvalid ranges ticket = not (null (invalidFields ranges ticket))

possible :: [[Integer]] -> [[Int]] -> [Integer] -> [[Int]]
possible _ [] [] = []
possible ranges (f:fields) (t:ticket) = f' : possible ranges fields ticket
  where f' = filter (\i -> t `elem` (ranges!!i)) f

helper :: (Eq a) => [a] -> [a] -> [a]
helper x y
  | x == y    = x
  | otherwise = x \\ y

reduce :: [[Int]] -> [[Int]] -> [[Int]]
reduce p [] = p
reduce p (r:rs)
  | length r == 1 = reduce (map (`helper` r) p) rs
  | otherwise     = reduce p rs

part2 :: Int -> [[Int]] -> [[Int]]
part2 n p
  | n == 0    = p
  | otherwise = part2 (n-1) p'
  where p' = reduce p p

main = do
  putStrLn "Day 16"
  f <- readFile "../input/input16.txt"
  let ls = lines f
  let ranges = map parseRange (take 20 ls)
  let me = map read (splitOn "," (ls !! 22)) :: [Integer]
  let near =  map (map read . splitOn ",") (drop 25 ls) :: [[Integer]]

  let part1 = concatMap (invalidFields ranges) near

  let valid = filter (not . isInvalid ranges) near
  let poss = replicate 20 [0..19] :: [[Int]]
  let s = foldl' (possible ranges) poss valid 
  let s' = concat $ part2 20 s

  printSoln 1 $ sum part1
  printSoln 2 $ product $ map snd $ filter (\(x,_) -> x < 6) $ zip s' me
