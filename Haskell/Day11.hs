import AdventHelper

import qualified Data.Map as Map
-- ----------------------------------------------------------------------------
dirs :: [(Int,Int)]
dirs = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]

data SeatCode = OPEN | FULL | FIX | OOB deriving (Eq, Show)

type Seats = Map.Map (Int,Int) SeatCode

parseInput :: Int -> [String] -> Seats
parseInput _ [] = Map.empty :: Seats
parseInput n (s:ss) = Map.union l (parseInput (n+1) ss)
  where l = Map.fromList $ parseLine n 0 s

parseLine :: Int -> Int -> String -> [((Int, Int), SeatCode)]
parseLine _ _ [] = []
parseLine r c (s:ss)
  | s == '.' = [((r,c), FIX)]  ++ (parseLine r (c+1) ss)
  | s == 'L' = [((r,c), OPEN)] ++ (parseLine r (c+1) ss)
  | s == '#' = [((r,c), FULL)] ++ (parseLine r (c+1) ss)

nbrs :: Seats -> (Int,Int) -> Int
nbrs seats (x,y) = length $ filter (== FULL) k
  where shell = map (\(dx, dy) -> (x+dx, y+dy)) dirs 
        k = map (\k -> Map.findWithDefault OOB k seats) shell

update :: Seats -> Seats
update seats = Map.mapWithKey (newChar seats) seats

updateUntilStable :: Seats -> Int
updateUntilStable seats
  | cur == nxt = cur
  | otherwise  = updateUntilStable seats'
  where seats' = update seats
        cur = length $ filter (== FULL) $ Map.elems seats
        nxt = length $ filter (== FULL) $ Map.elems seats'

newChar :: Seats -> (Int, Int) -> SeatCode -> SeatCode
newChar seats pos code
  | code == FIX = FIX
  | code == OPEN = if' (count == 0) FULL OPEN
  | code == FULL = if' (count >= 4) OPEN FULL
  where count = nbrs seats pos
------------------------- part 2 -----------------------------------------------
visNbrsSingle :: Seats -> (Int,Int) -> (Int, Int) -> Int
visNbrsSingle seats (x,y) (dx, dy)
  | icon == OOB = 0 -- we've gone off the end fo the grid
  | icon == OPEN = 0
  | icon == FULL = 1
  | icon == FIX = visNbrsSingle seats pos (dx, dy)
  where pos  = (x+dx, y+dy)
        icon = Map.findWithDefault OOB pos seats 

visNbrs :: Seats -> (Int,Int) -> Int
visNbrs seats pos = sum $ map (visNbrsSingle seats pos) dirs

newChar2 :: Seats -> (Int, Int) -> SeatCode -> SeatCode
newChar2 seats pos code
  | code == FIX = FIX
  | code == OPEN = if' (count == 0) FULL OPEN
  | code == FULL = if' (count >= 5) OPEN FULL
  where count = visNbrs seats pos

update2 :: Seats -> Seats
update2 seats = Map.mapWithKey (newChar2 seats) seats

updateUntilStable2 :: Seats -> Int
updateUntilStable2 seats
  | cur == nxt = cur
  | otherwise  = updateUntilStable2 seats'
  where seats' = update2 seats
        cur = length $ filter (== FULL) $ Map.elems seats
        nxt = length $ filter (== FULL) $ Map.elems seats'

main = do
  putStrLn "Day 11"
  f <- readFile "../input/input11.txt"
  let m = parseInput 0 $ lines f

  printSoln 1 (updateUntilStable m)
  printSoln 2 (updateUntilStable2 m)
