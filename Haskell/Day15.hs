import AdventHelper

import qualified Data.Map as Map
import Data.Maybe

type Game = Map.Map Int Int

tick :: (Game, Int, Int, Bool) -> (Game, Int, Int, Bool)
tick (game, n, turn, seen) = (game', n', turn + 1, seen')
  where n' = if' seen (turn - 1 - fromJust (Map.lookup n game)) 0
        seen' = Map.member n' game
        game' = Map.insert n (turn - 1) game

playUntil :: Int -> (Game, Int, Int, Bool) -> (Game, Int, Int, Bool)
playUntil upto (gm, n, turn, seen)
  | Map.size gm == 0 = error "Spurious pattern match to force evaulation!"
  | turn >  upto = (gm, n, turn, seen)
  | otherwise    = playUntil upto $ tick (gm, n, turn, seen)

main = do
  putStrLn "Day 15"
  let input15 = [7,14,0,17,11,1,2]
  let seed = if' (0 `elem` input15) (last input15) 0

  let game = Map.fromList $ zip input15 [1,2..] :: Game
  let gameState = (game, seed, 1 + length input15, 0 `elem` input15)

  let (_, part1, _, _) = playUntil 2020 gameState
  printSoln 1 part1

  let (_, part2, _, _) = playUntil 30000000 gameState
  printSoln 2 part2
