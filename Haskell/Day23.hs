import AdventHelper

import Data.List
import qualified Data.IntMap.Strict as IM
import Data.Maybe

type Game = IM.IntMap Int

buildGame :: Int -> [Int] -> Game
buildGame size input = IM.fromList $ zip numbers (drop 1 numbers ++ [head numbers])
  where numbers = input ++[(length input + 1), (length input + 2)..size]

seqMaxMod :: Int -> Int -> [Int] -> Int
seqMaxMod m x bad
  | x == 0       = seqMaxMod m m bad
  | x `elem` bad = seqMaxMod m (x-1) bad
  | otherwise    = x

imTick :: Int -> (Game, Int) -> (Game, Int)
imTick mx (g, cur) = (g', fromJust $ IM.lookup cur g')
  where (Just nbr1) = IM.lookup cur g
        (Just nbr2) = IM.lookup nbr1 g
        (Just nbr3) = IM.lookup nbr2 g
        (Just nbr4) = IM.lookup nbr3 g
        dest = seqMaxMod mx (cur-1) [nbr1, nbr2, nbr3]
        (Just afterDest) = IM.lookup dest g
        g' = foldl' (\m (k,v) -> IM.insert k v m) g [(cur, nbr4), (dest, nbr1), (nbr3, afterDest)]

imRun :: Int -> Int -> (Game, Int) -> (Game, Int)
imRun 0 _ xs = xs
imRun n m xs = imRun (n - 1) m xs'
  where xs' = imTick m xs

extract :: Int -> Int -> Game -> String
extract n i g 
  | i' == n = ""
  | otherwise = show i' ++ extract n i' g
  where (Just i') = IM.lookup i g

part2 :: Game -> Int
part2 g = val1 * val2
  where (Just val1) = IM.lookup 1 g
        (Just val2) = IM.lookup val1 g

main :: IO ()
main = do
  putStrLn "Day 23"
  let s = [5,2,3,7,6,4,8,1,9]

  let s1' = buildGame 9 s
  let k = imRun 100 (IM.size s1') (s1', head s)
  printSoln 1 $ extract 1 1 $ fst k

  let s2' = buildGame 1000000 s
  let k2 = imRun 10000000 (IM.size s2') (s2', head s)
  printSoln 2 $ part2 $ fst k2
