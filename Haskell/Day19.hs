import AdventHelper

import Data.List.Split
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.Set as Set


type CodeID = Integer
type Rule = (CodeID , [[CodeID]])
type Rules = Map.Map CodeID [[CodeID]]

-- [[str1, str2], [str3, str4], [str5]]
procString :: [[String]] -> [[CodeID]]
procString = foldr (\ s -> (++) [map read s :: [Integer]]) []

-- 61: 46 125 | 85 121
parseToRule :: String -> Rule
parseToRule ss
  | pairs == "a" = (i, [[-1]])
  | pairs == "b" = (i, [[-2]])
  | otherwise    = (i, rs)
  where i = read $ takeWhile (/= ':') ss :: Integer
        pairs = filter (/= '\"') $ drop 1 $ dropWhile (/= ' ') ss
        ps = map (splitOn " ") $ splitOn " | " pairs  -- [[str1, str2], [str3, str4]]
        rs = procString ps

ruleLookUp :: Rules -> CodeID -> [[CodeID]]
ruleLookUp rs i
  | i == -1   = [[-1]]
  | i == -2   = [[-2]]
  | otherwise = fromJust $ Map.lookup i rs

allProduct :: [[a]] -> [[a]]
allProduct []     = [[]]
allProduct (x:xs) = concatMap (\k -> map (k:) rest) x
  where rest = allProduct xs

expand :: Rules -> [Integer] -> Set.Set [Integer]
expand rs word = Set.fromList $ map concat $ allProduct lk
  where lk = map (ruleLookUp rs) word

expandWithMax :: Rules -> Int -> [Integer] -> Set.Set [Integer]
expandWithMax rs m word
  | length word > m = Set.empty
  | otherwise = Set.fromList $ map concat $ allProduct lk
  where lk = map (ruleLookUp rs) word

fullCollapse :: Rules -> Set.Set [Integer] -> Set.Set [Integer]
fullCollapse rs xs
  | all (all (< 0)) xs = xs
  | otherwise          = fullCollapse rs nxt
  where nxt = Set.unions $ Set.map (expand rs) xs

partialCollapse :: Int -> Rules -> Int -> Set.Set [Integer]-> Set.Set [Integer]
partialCollapse n rs m xs
  | n <= 0             = xs -- fail safe
  | all (all (< 0)) xs = xs
  | otherwise          = partialCollapse (n-1) rs m nxt
  where nxt = Set.unions $ Set.map (expandWithMax rs m) xs

deParse :: [Integer] -> String
deParse [] = ""
deParse (x:xs)
  | x == -1   = "a" ++ deParse xs
  | x == -2   = "b" ++ deParse xs
  | otherwise = "X" ++ deParse xs


fn :: (Int, Int) -> [Integer]
fn (m, n) = (replicate m 42) ++ (replicate n 42) ++ (replicate n 31)

part2Seed :: Int -> Int -> Set.Set [Integer]
part2Seed m n = Set.fromList $ map fn [ (x,y) | x <- [1..m], y <- [1..n]]

main = do
  putStrLn "Day 19"
  f <- readFile "../input/input19.txt"
  let l = take 133 $ lines f
  let targets = Set.fromList $ drop 134 $ lines f
  -- let l = ["42: 9 14 | 10 1",
  --          "9: 14 27 | 1 26",
  --          "10: 23 14 | 28 1",
  --          "1: a",
  --          "11: 42 31",
  --          "5: 1 14 | 15 1",
  --          "19: 14 1 | 14 14",
  --          "12: 24 14 | 19 1",
  --          "16: 15 1 | 14 14",
  --          "31: 14 17 | 1 13",
  --          "6: 14 14 | 1 14",
  --          "2: 1 24 | 14 4",
  --          "0: 8 11",
  --          "13: 14 3 | 1 12",
  --          "15: 1 | 14",
  --          "17: 14 2 | 1 7",
  --          "23: 25 1 | 22 14",
  --          "28: 16 1",
  --          "4: 1 1",
  --          "20: 14 14 | 1 15",
  --          "3: 5 14 | 16 1",
  --          "27: 1 6 | 14 18",
  --          "14: b",
  --          "21: 14 1 | 1 14",
  --          "25: 1 1 | 1 14",
  --          "22: 14 14",
  --          "8: 42",
  --          "26: 14 22 | 1 20",
  --          "18: 15 15",
  --          "7: 14 5 | 1 21",
  --          "24: 14 1"]
  -- let targets = Set.fromList $
  --               ["abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa",
  --                "bbabbbbaabaabba",
  --                "babbbbaabbbbbabbbbbbaabaaabaaa",
  --                "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
  --                "bbbbbbbaaaabbbbaaabbabaaa",
  --                "bbbababbbbaaaaaaaabbababaaababaabab",
  --                "ababaaaaaabaaab",
  --                "ababaaaaabbbaba",
  --                "baabbaaaabbaaaababbaababb",
  --                "abbbbabbbbaaaababbbbbbaaaababb",
  --                "aaaaabbaabaaaaababaa",
  --                "aaaabbaaaabbaaa",
  --                "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
  --                "babaaabbbaaabaababbaabababaaab",
  --                "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"]

  let rs = Map.fromList $ map parseToRule l

  let mx = Set.foldl max 0 $ Set.map length targets
  putStrLn $ show mx

  let zero = Set.fromList $ fromJust $ Map.lookup 0 rs
  let fc = Set.map deParse $ fullCollapse rs zero
  putStrLn $ show $ Set.size fc
  printSoln 1 $ Set.size (Set.intersection targets fc)

  -- let newZero = part2Seed 2 2
  -- putStrLn $ show newZero
  -- let fc = Set.map deParse $ partialCollapse 10 rs mx newZero
  -- putStrLn $ show $ Set.size fc
  -- putStrLn $ show $ Set.take 10 fc
  -- printSoln 2 $ Set.size (Set.intersection targets fc)



  -- let rs' = Map.insert 11 [[42, 31], [42, 11, 31]] $ Map.insert 8 [[42], [42, 8]] rs

