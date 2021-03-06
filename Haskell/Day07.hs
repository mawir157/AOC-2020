import AdventHelper

import Data.List
import Data.List.Split

type Bag = (String, [(Integer, String)])

parseInput :: String -> (String, [(Integer, String)])
parseInput s = ((a++" "++c), parseChildren cs)
 where [p,cs] = splitOn " contain " s
       [a,c,_] = splitOn " " p

parseChildren :: String -> [(Integer, String)]
parseChildren "no other bags." = []
parseChildren "" = []
parseChildren s = [(n, (a++" "++c))] ++ parseChildren next
  where [i,a,c,_] = splitOn " " $ takeWhile (/= ',') s
        n = read i :: Integer
        next = drop 2 $ dropWhile (/= ',') s

replace :: (Integer, Bag) -> Bag -> (Integer, Bag)
replace (i, (b, bs)) (t, ts)
  | bs == []  = (i, (b, bs))
  | otherwise = (i + rep, (b, sort (bs' ++ ts')))
  where rep = sum $ map (fst) $ filter (\(_,cs) -> cs == t) bs
        bs' = filter (\(_,cs) -> cs /= t) bs
        ts' = map (\(v,w) -> (rep*v, w)) ts

collapse' :: [Bag] -> (Integer, Bag) -> (Integer, Bag)
collapse' bs (i, b)
  | b == b'   = (i, b)
  | otherwise = collapse' bs (i', b')
  where (i', b') = foldl replace (i, b) bs

upstream :: [Bag] -> String -> [String]
upstream bs s = map (fst) $ filter (helper s) bs
  where helper s' (_, cs) = elem s' $ map (snd) cs

main = do
  putStrLn "Day 7"
  f <- readFile "../input/input07.txt"
  let s = map(parseInput) $ lines f
  let r = iterate (nub . concat . map (upstream s)) ["shiny gold"]
  printSoln 1 (length $ nub $ concat $ drop 1 $ takeWhile (/= []) r)

  let sg = head $ filter (\x -> fst x == "shiny gold") s
  let j = collapse' s (0,sg)
  printSoln 2 ((fst j))
