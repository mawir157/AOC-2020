import AdventHelper

import qualified Data.Set as Set

type Point = [Int]
type Space = Set.Set Point

parseInput :: Int -> Int -> [String] -> Space
parseInput _  _ [] = Set.empty :: Space
parseInput dim n (s:ss) = Set.union l (parseInput dim (n+1) ss)
  where l = Set.fromList $ parseLine dim n 0 s

parseLine :: Int -> Int -> Int -> String -> [[Int]]
parseLine _ _ _ [] = []
parseLine dim r c (s:ss)
  | s == '#'  = ([r,c] ++ zeros) : parseLine dim r (c + 1) ss
  | otherwise = [] ++ parseLine dim r (c + 1) ss
  where zeros = replicate (dim-2) 0

dirs3 :: Space
dirs3 = Set.fromList $ filter (/= [0,0,0]) [ [x,y,z] |
                                    x <- [-1,0,1], y <- [-1,0,1], z <- [-1,0,1]]

dirs4 :: Space
dirs4  = Set.fromList $ filter (/= [0,0,0,0]) [ [x,y,z,t] |
                     x <- [-1,0,1], y <- [-1,0,1], z <- [-1,0,1], t <- [-1,0,1]]

nbrs :: Point -> Space
nbrs [x,y,z] = Set.map (\[dx,dy,dz] -> [x+dx,y+dy,z+dz]) dirs3
nbrs [x,y,z,t] = Set.map (\[dx,dy,dz,dt] -> [x+dx,y+dy,z+dz,t+dt]) dirs4
nbrs _ = error "Unexpected dimension"

nbrsCount :: Space -> Point-> Int
nbrsCount sp pt = Set.size (Set.intersection (nbrs pt) sp)

toCheck :: Space -> Space
toCheck s = Set.union s $ Set.unions $ Set.map nbrs s

update :: Space -> Point -> Bool
update sp pt
  | Set.member pt sp = n == 2 || n == 3
  | otherwise        = n == 3
  where n = nbrsCount sp pt

tick :: Space -> Space
tick sp = Set.filter (update sp) (toCheck sp)

run :: Integer -> Space -> Space
run n sp
  | n == 0    = sp
  | otherwise = run (n-1) $ tick sp

main = do
  putStrLn "Day 17"
  f <- readFile "../input/input17.txt"
  let sp3 = parseInput 3 0 $ lines f
  let sp4 = parseInput 4 0 $ lines f

  printSoln 1 $ Set.size $ run 6 sp3
  printSoln 2 $ Set.size $ run 6 sp4
