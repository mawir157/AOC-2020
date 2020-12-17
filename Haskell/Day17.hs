import AdventHelper

import qualified Data.Set as Set

type Point = (Int,Int,Int)
type Space = Set.Set Point

type Point4 = (Int,Int,Int,Int)
type Space4 = Set.Set Point4

-- ----------------------------------------------------------------------------
dirs :: Space
dirs = Set.fromList $ filter (/= (0,0,0)) [ (x,y,z) |
                                    x <- [-1,0,1], y <- [-1,0,1], z <- [-1,0,1]]

nbrs :: Point -> Space
nbrs (x,y,z) = Set.map (\(dx,dy,dz) -> (x+dx,y+dy,z+dz)) dirs

nbrsCount :: Space -> Point -> Int
nbrsCount sp pt = Set.size (Set.intersection ns sp)
  where ns = nbrs pt

toCheck :: Space -> Space
toCheck s = Set.union s $ Set.unions $ Set.map nbrs s

update :: Space -> Point -> Bool
update sp pt
  | Set.member pt sp = n == 2 || n == 3
  | otherwise        = n == 3
  where n = nbrsCount sp pt

tick :: Space -> Space
tick sp = Set.filter (update sp) sp'
  where sp' = toCheck sp

run :: Integer -> Space -> Space
run n sp
  | n == 0    = sp
  | otherwise = run (n-1) $ tick sp
--------------------------------------------------------------------------------
dirs4 :: Space4
dirs4 = Set.fromList $ filter (/= (0,0,0,0)) [ (x,y,z,t) |
                  x <- [-1,0,1], y <- [-1,0,1], z <- [-1,0,1], t <- [-1,0,1]]

nbrs4 :: Point4 -> Space4
nbrs4 (x,y,z,t) = Set.map (\(dx,dy,dz,dt) -> (x+dx,y+dy,z+dz,t+dt)) dirs4

nbrsCount4 :: Space4 -> Point4 -> Int
nbrsCount4 sp pt = Set.size (Set.intersection ns sp)
  where ns = nbrs4 pt

toCheck4 :: Space4 -> Space4
toCheck4 s = Set.union s $ Set.unions $ Set.map nbrs4 s

update4 :: Space4 -> Point4 -> Bool
update4 sp pt
  | Set.member pt sp = n == 2 || n == 3
  | otherwise        = n == 3
  where n = nbrsCount4 sp pt

tick4 :: Space4 -> Space4
tick4 sp = Set.filter (update4 sp) sp'
  where sp' = toCheck4 sp

run4 :: Integer -> Space4 -> Space4
run4 n sp
  | n == 0    = sp
  | otherwise = run4 (n-1) $ tick4 sp
--------------------------------------------------------------------------------
parseInput :: Int -> [String] -> Space
parseInput _ [] = Set.empty :: Space
parseInput n (s:ss) = Set.union l (parseInput (n+1) ss)
  where l = Set.fromList $ parseLine n 0 s

parseLine :: Int -> Int -> String -> [(Int, Int,Int)]
parseLine _ _ [] = []
parseLine r c (s:ss)
  | s == '#'  = (r, c, 0) : parseLine r (c + 1) ss
  | otherwise = [] ++ parseLine r (c + 1) ss

main = do
  putStrLn "Day 17"
  f <- readFile "../input/input17.txt"
  let sp = parseInput 0 $ lines f
  let sp4 = Set.map (\(x,y,z) -> (x,y,z,0)) sp

  printSoln 1 $ Set.size $ run 6 sp
  printSoln 2 $ Set.size $ run4 6 sp4
