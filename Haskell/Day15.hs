import AdventHelper

import qualified Data.Map as Map
import Data.Maybe

type Game = Map.Map Int [Int]

history :: [Int] -> [Int] -> [Int]
history new old = new ++ take 1 old

tick :: (Game, Int, Int) -> (Game, Int, Int)
tick (gm, turn, prev) = (gm', turn+1, cur)
  where hist = fromJust $ Map.lookup prev gm
        cur = if' (length hist == 1) (0) (hist!!0 - hist!!1)
        gm' = Map.insertWith history cur [turn]  gm

playUntil :: Int -> (Game, Int, Int) -> (Game, Int, Int)
playUntil n (gm, turn, prev)
  | turn >  n = (gm, turn, prev)
  | otherwise = playUntil n $! next
  where next = tick (gm, turn, prev)

shatter :: [a] -> [[a]]
shatter = foldr (\ x -> (++) [[x]]) []

main = do
  putStrLn "Day 15"
  let input15 = [7,14,0,17,11,1,2]
  -- let input15 = [0,3,6]
  let game = Map.fromList $ zip input15 (shatter [1,2..]) :: Game
  let gameState = (game, length input15 + 1, last input15)

  let (_, _, prev) = playUntil 2020 gameState
  printSoln 1 prev

  let (_, _, prev2) = playUntil 3000000 gameState
  printSoln 2 prev2
