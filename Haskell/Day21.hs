import AdventHelper

import Data.Function (on)
import Data.List
import Data.List.Split
import qualified Data.Set as Set

type Food = (Set.Set String, Set.Set String)

parseInput :: String -> Food
parseInput ss = (is, as)
  where tt = splitOn " (contains " ss
        is = Set.fromList $ splitOn " " (tt!!0)
        as = Set.fromList $ splitOn ", " (init (tt!!1))

addFood :: (Set.Set String, Set.Set String) -> Food -> (Set.Set String, Set.Set String)
addFood (is, as) (i,a) = (Set.union is i, Set.union as a)

foodsWAll :: [Food] -> String -> [Food]
foodsWAll fs a = filter (\(_,as) -> Set.member a as) fs

foodCollect :: [Food] -> (Set.Set String, Set.Set String)
foodCollect = foldl' addFood (Set.empty, Set.empty)

reduceAllegen :: [Food] -> String -> Set.Set String
reduceAllegen fs a = foldl1 Set.intersection red
  where red = map fst $ foodsWAll fs a

removeAlg :: [Food] -> (String, Set.Set String) -> [Food]
removeAlg fs (a, is) = map (\(j, b) -> (Set.difference j is, Set.delete a b)) fs

removeIng :: [Food] -> String -> [Food]
removeIng fs i = map (\(is,as) -> (Set.delete i is, as)) fs

kill :: [Food] -> [Food]
kill fs = foldl' removeAlg fs killable
  where (_, as) = foodCollect fs
        q = Set.map (\x -> (x, reduceAllegen fs x)) as
        killable = Set.filter (\x -> Set.size (snd x) == 1) q

killRep :: [Food] -> [Food]
killRep fs
  | Set.size as == 0 = fs
  | otherwise        = killRep $ kill fs
  where (_, as) = foodCollect fs

stripIngredients :: ([Food], [(String,String)]) -> ([Food], [(String,String)])
stripIngredients (fs, seen) = (foldl' removeAlg fs removable, seen')
  where (_, as) = foodCollect fs
        q = Set.map (\x -> (x, reduceAllegen fs x)) as
        removable = Set.filter (\x -> Set.size (snd x) == 1) q
        seen' = seen ++ map (\ (x, y) -> (x, Set.elemAt 0 y)) (Set.toList removable)

stripRep :: ([Food], [(String,String)]) -> ([Food], [(String,String)])
stripRep (fs, seen)
  | Set.size as == 0 = (fs, seen)
  | otherwise        = stripRep $ stripIngredients (fs, seen)
  where (_, as) = foodCollect fs

main :: IO ()
main = do
  putStrLn "Day 21"
  f <- readFile "../input/input21.txt"
  let s = lines f

  let menu = map parseInput s
  let clean = concatMap (\(x,_) -> Set.toList x) $ killRep menu
  let cleanFood = foldl' removeIng menu clean
  let (_, dict) = stripRep (cleanFood, [])
  let sortedDict = sortBy  (compare `on` fst) dict

  printSoln 1 $ length clean
  printSoln 2 $ concat' (map snd sortedDict) ","
