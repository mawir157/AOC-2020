import AdventHelper

import Data.List
import Data.Maybe
import qualified Data.Map as Map

type Seen = Map.Map Integer Integer

joltChain :: [Integer] -> ([Integer],[Integer]) -> ([Integer],[Integer])
joltChain js (diffs, chain)
  | length cands > 0 = joltChain js (diffs ++ [nx-c], chain ++ [nx])
  | otherwise        = (diffs ++ [3], chain ++ [c+3])
  where c     = last chain
        cands = filter (\x ->  elem x [(c+1)..(c+3)]) js
        nx    = minimum cands

allJoltChains :: [Integer] -> Integer -> Integer -> Integer
allJoltChains js to from
  | to - from <= 3 = 1
  | otherwise      = sum $ map (allJoltChains js to) cands
  where cands = intersect js [(from+1)..(from+3)]

routesTo :: Integer -> ([Integer], Seen) -> ([Integer], Seen)
routesTo to ((j:js), seen)
  | to - j <= 3 = (js, (Map.insert j 1 seen))
  | otherwise   = (js, (Map.insert j (sum t) seen))
  where t = filter (/= (-1)) $ map (\x -> Map.findWithDefault (-1) x seen) [(j+1)..(j+3)]

main = do
  putStrLn "Day 10"
  f <- readFile "../input/input10.txt"
  let js = reverse $ sort $ map (read) $ lines f :: [Integer]

  let (diffs,chain) = joltChain js ([minimum js], [minimum js])
  printSoln 1 ((length $ filter (3 ==) diffs) * (length $ filter (1 ==) diffs))

  let s = iterate (routesTo (3 + maximum js)) (js ++ [0], Map.empty)
  let m = snd $ head $ dropWhile (\x -> fst x /= []) s
  printSoln 2 (fromJust $ Map.lookup 0 m)
