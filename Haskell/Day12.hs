import AdventHelper

import Data.List 

type Ins = (Char, Integer)
type Pos = (Integer, Integer)
type Robot = (Integer, Pos)

parseLine :: String -> (Char, Integer)
parseLine (s:ss) = (s, n)
  where n = read ss :: Integer
--------------------------------------------------------------------------------
moveRobot :: Robot -> Ins -> Robot
moveRobot (dir, (x,y)) (c, val)
  | c == 'N' = (dir, (x, y + val))
  | c == 'S' = (dir, (x, y - val))
  | c == 'E' = (dir, (x + val, y))
  | c == 'W' = (dir, (x - val, y))
  | c == 'L' = (mod (dir + (div val 90)) 4, (x, y))
  | c == 'R' = (mod (dir - (div val 90)) 4, (x, y))
  | c == 'F' = forward (dir, (x,y)) (c, val)

forward :: Robot -> Ins -> Robot
forward rbt (_, val)
  | fst rbt == 0 = moveRobot rbt ('E', val)
  | fst rbt == 1 = moveRobot rbt ('N', val)
  | fst rbt == 2 = moveRobot rbt ('W', val)
  | fst rbt == 3 = moveRobot rbt ('S', val)
--------------------------------------------------------------------------------
rot :: Pos -> Ins -> Pos
rot (x,y) (c, val)
  | val == 180 = (-x, -y)
  | ((c, val) == ('L', 90)) || ((c, val) == ('R', 270)) = (-y, x)
  | ((c, val) == ('R', 90)) || ((c, val) == ('L', 270)) = (y, -x)

relativeMove :: (Pos, Pos) -> Ins -> (Pos, Pos) 
relativeMove ((wx, wy), (sx, sy)) (c, val)
  | c == 'N' = ((wx, wy + val), (sx, sy))
  | c == 'S' = ((wx, wy - val), (sx, sy))
  | c == 'E' = ((wx + val, wy), (sx, sy))
  | c == 'W' = ((wx - val, wy), (sx, sy))
  | c == 'F' = ((wx, wy), (sx + val * wx, sy + val * wy))
  | c == 'L' || c == 'R' = (rot (wx, wy) (c, val), (sx, sy))

main = do
  putStrLn "Day 12"
  f <- readFile "../input/input12.txt"
  let m = map parseLine $ lines f

  let q = foldl' moveRobot (0, (0, 0)) m
  let t = foldl' relativeMove ((10, 1), (0,0)) m

  printSoln 1 ((abs$fst$snd q) + (abs$snd$snd q))
  printSoln 2 ((abs$fst$snd t) + (abs$snd$snd t))
