import AdventHelper

import Data.List
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

type Tile = (Integer, [String])

type Grid = Map.Map (Integer, Integer) Tile
type Pieces = Set.Set Tile

stringToBin :: String -> [Integer]
stringToBin s = bin
  where bin = map (\x -> if' (x == '#') 1 0) s

parseInput :: [String] -> Tile
parseInput [] = error "Bad Input"
parseInput (s:ss) = (code, ss)
  where code = read (take 4 $ drop 5 s) :: Integer

rot :: Tile -> Tile
rot (code, ss) = (code, rotateHelper ss)
 
rotateHelper :: [String] -> [String]
rotateHelper ss 
  | null (head ss) = []
  | otherwise      = reverse (map head ss) : rotateHelper (map tail ss)

flp :: Tile -> Tile
flp (code, ss) = (code, map reverse ss)

allIsoms :: Tile -> [Tile]
allIsoms t = [t,           rot t,       rot $ rot t,       rot $ rot $ rot t,
              flp t, flp $ rot t, flp $ rot $ rot t, flp $ rot $ rot $ rot t]

tileCode :: Tile -> Integer
tileCode (code, _) = code

deleteCode :: Tile -> Pieces -> Pieces
deleteCode t ps = Set.difference ps mt
  where mt = Set.filter (\e -> tileCode e == tileCode t) ps

tileData :: Tile -> [String]
tileData (_, ss) = ss

top :: Tile -> [Integer]
top (_, ss) = stringToBin $ head ss

left :: Tile -> [Integer]
left (_, ss) = stringToBin $ map head ss

bottom :: Tile -> [Integer]
bottom (_, ss) = stringToBin $ last ss

right :: Tile -> [Integer]
right (_, ss) = stringToBin $ map last ss

joinBelow :: Tile -> Tile -> Bool
joinBelow t1 t2 = (tileCode t1 /= tileCode t2) && (bottom t1 == top t2)

joinRight :: Tile -> Tile -> Bool
joinRight t1 t2 = (tileCode t1 /= tileCode t2) && (right t1 == left t2)

joinAbove :: Tile -> Tile -> Bool
joinAbove t1 t2 = (tileCode t1 /= tileCode t2) && (top t1 == bottom t2)

joinLeft :: Tile -> Tile -> Bool
joinLeft t1 t2 = (tileCode t1 /= tileCode t2) && (left t1 == right t2)

belowNbrs :: Set.Set Tile -> Tile -> Pieces
belowNbrs ts t = Set.fromList $ filter (joinBelow t) allPerms
  where allPerms = concatMap allIsoms ts

rightNbrs :: Set.Set Tile -> Tile -> Pieces
rightNbrs ts t = Set.fromList $ filter (joinRight t) allPerms
  where allPerms = concatMap allIsoms ts

aboveNbrs :: Set.Set Tile -> Tile -> Pieces
aboveNbrs ts t = Set.fromList $ filter (joinAbove t) allPerms
  where allPerms = concatMap allIsoms ts

leftNbrs :: Set.Set Tile -> Tile -> Pieces
leftNbrs ts t = Set.fromList $ filter (joinLeft t) allPerms
  where allPerms = concatMap allIsoms ts

countNbrs :: Set.Set Tile -> Tile -> [Int]
countNbrs ts t =  [Set.size $ belowNbrs ts t, Set.size $ rightNbrs ts t,
                   Set.size $ aboveNbrs ts t, Set.size $ leftNbrs  ts t]
---- THIS CAN ALL BE COLLAPSED TO REMOVE DUPLICATION 
---- BUT I AM TOO STUPID TO DO THIS ATM
openRight :: Integer -> Grid -> [(Integer, Integer)]
openRight m gr = nxt \\ ky
  where ky = Map.keys gr
        nxt = filter (\(x,_) -> x < m) $ map (\(x,y) -> (x+1, y)) ky

openBelow :: Integer -> Grid -> [(Integer, Integer)]
openBelow m gr = nxt \\ ky
  where ky = Map.keys gr
        nxt = filter (\(_,y) -> y < m) $ map (\(x,y) -> (x, y+1)) ky

addRightInt :: (Grid, Pieces) -> (Integer, Integer) -> (Grid, Pieces)
addRightInt (gd, ps) (x,y) = (Map.insert (x,y) toAdd gd, deleteCode toAdd ps)
  where nbr = Maybe.fromJust $ Map.lookup (x-1, y) gd
        toAdd = Set.elemAt 0 $ rightNbrs ps nbr

addRight :: (Grid, Pieces) -> (Grid, Pieces)
addRight (gd, ps)
  | not (null open) = addRight $ foldl' addRightInt (gd, ps) open
  | otherwise       = (gd, ps)
  where open = openRight 12 gd -- TODO

addBelowInt :: (Grid, Pieces) -> (Integer, Integer) -> (Grid, Pieces)
addBelowInt (gd, ps) (x,y) = (Map.insert (x,y) toAdd gd, deleteCode toAdd ps)
  where nbr = Maybe.fromJust $ Map.lookup (x, y-1) gd
        toAdd = Set.elemAt 0 $ belowNbrs ps nbr

addBelow :: (Grid, Pieces) -> (Grid, Pieces)
addBelow (gd, ps)
  | not (null open) = addBelow $ foldl' addBelowInt (gd, ps) open
  | otherwise       = (gd, ps)
  where open = openBelow 12 gd -- TODO
--------------------------------------------------------------------------------
collapseRow :: Integer -> Grid -> [String]
collapseRow n gd = collapseHelper tiles
  where indices = [ (x, n) | x <- [0..11] ] -- TODO
        tiles = map (\t -> tileData$ Maybe.fromJust $ Map.lookup t gd) indices --[[String]]

collapseHelper :: [[String]] -> [String]
collapseHelper gr
  | length (gr!!0) == 0 = []
  | otherwise           = [foldl1 glue ss] ++ collapseHelper sn
  where ss = map head gr 
        sn = map tail gr

glue :: [a] -> [a] -> [a]
glue s1 s2 = (init s1) ++ (tail s2)

trim :: [String] -> [String]
trim ss = init $ tail ss'
  where ss' = map (init . tail) ss
--------------------------------------------------------------------------------
dragonAt :: String -> [String] -> Int -> (Int, Int) -> Bool
dragonAt rgx ss offset (r,c) = check =~ rgx
  where check = take 20 $ drop c (ss!!(r-offset))

dragonCount :: [String] -> Int
dragonCount gr = length d1
  where pairs = [ (r,c) | r <- [2..(length gr - 1)],  c <- [0..(length gr - 1)]]
        d3 = filter (\p -> dragonAt ".#.{2}#.{2}#.{2}#.{2}#.{2}#.{3}" gr 0 p) pairs
        d2 = filter (\p -> dragonAt "#.{4}##.{4}##.{4}###" gr 1 p) d3
        d1 = filter (\p -> dragonAt ".{18}#." gr 2 p) d2

countRough :: [String] -> Int
countRough [] = 0
countRough (s:ss) = (length $ filter (== '#') s) + countRough ss

main :: IO ()
main = do
  putStrLn "Day 20"
  f <- readFile "../input/input20.txt"
  let s = Set.fromList $ map (parseInput . init) $ chunksOf 12 $ lines f
  let corners = Set.filter (\x -> sum (countNbrs s x) == 2) s
  printSoln 1 $ product $ Set.map tileCode corners

  -- Part 2 
  -- pick an arbitrary corner and rotate it so it only has nbrs right and below
  let tls = take 4 $ iterate rot $ Set.elemAt 0 corners
  let topLeft = head $ filter (\t -> countNbrs s t == [1,1,0,0])  tls
  let grid = Map.singleton (0,0) topLeft -- insert this into the grid
  let pieces = Set.delete topLeft s -- and remove it from the remaining pieces

  -- fill the the top row then fill the columns
  let (grid', _) = addBelow $ addRight (grid, pieces)
  let pattern = trim $ foldl1 glue $ map (\n -> collapseRow n grid') [0..11] -- TODO
  let qpat = rotateHelper $ rotateHelper pattern

  printSoln 2 ((countRough pattern) - (15 * (dragonCount $ qpat)))
