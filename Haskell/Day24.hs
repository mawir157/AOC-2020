import AdventHelper

import Data.List
import qualified Data.Set as Set

type Cell = (Integer, Integer)
type HexGrid = Set.Set (Integer, Integer)

parseLine :: Cell -> String -> Cell
parseLine (x, y) [] = (x,y)
parseLine (x, y) [s]
  | s == 'e'      = (x+1, y)
  | s == 'w'      = (x-1, y)
parseLine (x, y) (s:t:ss)
  | s == 'e'      = parseLine (x+1, y)   (t:ss)
  | s == 'w'      = parseLine (x-1, y)   (t:ss)
  | [s,t] == "nw" = parseLine (x-1, y+1) (ss)
  | [s,t] == "ne" = parseLine (x, y+1)   (ss)
  | [s,t] == "sw" = parseLine (x, y-1)   (ss)
  | [s,t] == "se" = parseLine (x+1, y-1) (ss)
parseLine _ s = error s

toggleAdd :: HexGrid -> Cell -> HexGrid
toggleAdd s p
  | Set.member p s = Set.delete p s
  | otherwise      = Set.insert p s 

nbrs :: Cell -> HexGrid
nbrs (x,y) = Set.fromList [(x-1,y),(x+1,y),(x,y-1),(x,y+1),(x-1,y+1),(x+1,y-1)]

nbrsCount :: HexGrid -> Cell -> Int
nbrsCount sp pt = Set.size (Set.intersection (nbrs pt) sp)

toCheck :: HexGrid -> HexGrid
toCheck s = Set.union s $ Set.unions $ Set.map nbrs s

update :: HexGrid -> Cell -> Bool
update sp pt
  | Set.member pt sp = n == 1 || n == 2
  | otherwise        = n == 2
  where n = nbrsCount sp pt

tick :: HexGrid -> HexGrid
tick sp = Set.filter (update sp) (toCheck sp)

run :: Integer -> HexGrid -> HexGrid
run n sp
  | n == 0    = sp
  | otherwise = run (n-1) $ tick sp

main :: IO ()
main = do
  putStrLn "Day 24"
  f <- readFile "../input/input24.txt"
  let q = foldl' toggleAdd Set.empty $ map (parseLine (0,0)) $ lines f

  printSoln 1 $ Set.size q
  printSoln 2 $ Set.size $ run 100 q
