import AdventHelper

import Control.Monad
import Data.List.Split

import qualified Data.IntMap as IM

data Rule
  = Const Char  -- single character
  | Pair [Rule] -- pair of rules
  | Sing [Int]  -- single rule
  deriving (Show, Eq, Ord)

eval :: IM.IntMap Rule -> String -> Rule -> [String]
eval _     []          (Const _ ) = []
eval _     (c' : rest) (Const c ) = [ rest | c == c' ]
eval rules s           (Sing  ns) = foldM (eval rules) s (map (rules IM.!) ns)
eval rules s           (Pair  rs) = concat [ eval rules s r | r <- rs ]

parseRule :: String -> (Int, Rule)
parseRule ss
  | length rhs == 1 = (i, Const $ head rhs)
  | length ps  == 1 = (i, Sing $ head ps)
  | otherwise = (i, Pair $ map Sing ps)
  where i = read $ takeWhile (/= ':') ss :: Int
        rhs = filter (/= '\"') $ drop 1 $ dropWhile (/= ' ') ss
        ps = procString $ map (splitOn " ") $ splitOn " | " rhs

procString :: [[String]] -> [[Int]]
procString = foldr (\ s -> (++) [map read s :: [Int]]) []

main = do
  putStrLn "Day 19"
  f <- readFile "../input/input19.txt"
  let l = takeWhile (not . null) $ lines f
  let targets = drop 1 $ dropWhile (not . null) $ lines f

  let rs = IM.fromList $ map parseRule l

  let q = map (\t -> eval rs t (rs IM.! 0)) targets
  printSoln 1 $ length $ filter ([] `elem`) q

  let rs2 = IM.insert 8 (Pair [Sing [42], Sing [42, 8]])
             $ IM.insert 11 (Pair [Sing [42, 31], Sing [42, 11, 31]]) rs

  let q2 = map (\t -> eval rs2 t (rs2 IM.! 0)) targets
  printSoln 2 $ length $ filter ([] `elem`) q2
