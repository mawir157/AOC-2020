import AdventHelper

import Data.List
import Data.Maybe
import qualified Data.Map as Map

type Seen = Map.Map Integer Integer

routesTo :: Integer -> ([Integer], Seen) -> ([Integer], Seen)
routesTo to ((j:js), seen)
  | to - j <= 3 = (js, (Map.insert j 1 seen))
  | otherwise   = (js, (Map.insert j (sum t) seen))
  where t = filter (/= (-1)) $ map (\x -> Map.findWithDefault (-1) x seen) [(j+1)..(j+3)]

main = do
  putStrLn "Day 10"
  f <- readFile "../input/input10.txt"
  let js = reverse $ sort $ map (read) $ lines f :: [Integer]

  let d = diff ([3 + maximum js] ++ js ++ [0])
  printSoln 1 ((length $ filter (3 ==) d) * (length $ filter (1 ==) d))

  let s = iterate (routesTo (3 + maximum js)) (js ++ [0], Map.empty)
  let m = snd $ head $ dropWhile (\x -> fst x /= []) s
  printSoln 2 (fromJust $ Map.lookup 0 m)
