import AdventHelper

import Data.List
import Data.List.Split
import Data.Function (on)

helper :: String -> [Integer]
helper s
  | s == "x"  = []
  | otherwise = [read s :: Integer]

parseBuses :: String -> [Integer]
parseBuses s = concatMap helper ss
  where ss = splitOn "," s

helper2 :: (Integer, String) -> [(Integer, Integer)]
helper2 (i, s)
  | s == "x"  = []
  | otherwise = [(i, read s :: Integer)]

parseBuses2 :: String -> [(Integer, Integer)]
parseBuses2 s = concatMap helper2 pairs
  where ss = splitOn "," s
        pairs = zip [0,1..] ss

firstBus :: Integer -> [Integer] -> (Integer, Integer)
firstBus strt bs = minPair $ zip delays bs
  where mds = map (mod strt) bs
        delays = zipWith subtract mds bs

main = do
  putStrLn "Day 10"
  f <- readFile "../input/input13.txt"
  let ss = lines f
  let now = read (head ss) :: Integer

  let buses = parseBuses (last ss)
  let part1 = firstBus now buses

  let buses2 = sortBy (compare `on` snd) $ parseBuses2 (ss!!1)
  let part2 = foldl1 chiRemThm buses2

  printSoln 1 $ fst part1 * snd part1
  printSoln 2 $ snd part2 - fst part2
