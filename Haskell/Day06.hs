import AdventHelper

import Data.List
import Data.List.Split

eachAnswer :: String -> String
eachAnswer s = foldl1 intersect ss
  where ss = splitOn ":" s

main = do
  putStrLn "Day 6"
  f <- readFile "../input/input06.txt"
  let s = parseLineGroups ":" $ lines f

  printSoln 1 (sum $ map (length . nub . filter (/= ':')) s)
  printSoln 1 (sum $ map (length . nub . eachAnswer) s)
