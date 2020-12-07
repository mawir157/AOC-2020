import AdventHelper

import Data.List
import Data.List.Split

type Bag = (String, [(Integer, String)])

parseInput :: String -> (String, [(Integer, String)])
parseInput s = ((a++" "++c), parseChildren cs)
 where [p,cs] = splitOn " contain " s
       [a,c,b] = splitOn " " p

parseChildren :: String -> [(Integer, String)]
parseChildren "no other bags." = []
parseChildren "" = []
parseChildren s = [(n, (a++" "++c))] ++ parseChildren next
  where [i,a,c,b] = splitOn " " $ takeWhile (/= ',') s
        n = read i :: Integer
        next = drop 2 $ dropWhile (/= ',') s

replace :: (Integer, Bag) -> Bag -> (Integer, Bag)
replace (i, (b, bs)) (t, ts)
  | ts == []  = (i, (b, bs))
  | rep == 0  = (i, (b, bs))
  | otherwise = (i + rep, (b, sort (bs' ++ ts')))
  where rep = sum $ map (fst) $ filter (\(_,cs) -> cs == t) bs
        bs' = filter (\(_,cs) -> cs /= t) bs
        ts' = map (\(v,w) -> (rep*v, w)) ts

collapse :: [Bag] -> (Integer, Bag) -> (Integer, Bag)
collapse bs (i, cs) = foldl replace (i, cs) bs

collapse' :: [Bag] -> (Integer, Bag) -> (Integer, Bag)
collapse' bs (i, b)
  | b == b'   = (i, b)
  | otherwise = collapse' bs (i', b')
  where (i', b') = collapse bs (i, b)

helper :: String -> Bag ->  Bool
helper s (_, cs) = elem s cs'
  where cs' = map (snd) cs

upstream :: [Bag] -> String -> [String]
upstream bs s = map (fst) $ filter (helper s) bs

main = do
  putStrLn "Day 7"
  f <- readFile "../input/input07.txt"
  let s = map(parseInput) $ lines f
  let r = iterate (nub . concat . map (upstream s)) ["shiny gold"]
  printSoln 1 (length $ nub $ concat $ drop 1 $ takeWhile (/= []) r)

  let sg = head $ filter (\x -> fst x == "shiny gold") s
  let j = collapse' s (0,sg)
  let k = sum $ map (fst) $ snd $ snd j
  printSoln 2 ((fst j) + k)
