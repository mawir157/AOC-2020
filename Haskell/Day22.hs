import AdventHelper

type Game = ([Int], [Int])

parseInput :: [String] -> [[Int]]
parseInput [] = []
parseInput ss = deck : parseInput ss'
  where deck = map read $ takeWhile (not . null) $ drop 1 ss :: [Int]
        ss' = drop 1 $ dropWhile (not . null) ss

tick :: Game -> Game
tick ([], x) = ([], x)
tick (x, []) = (x, [])
tick (s : ss, t : tt)
  | s > t = (ss ++ [s] ++ [t], tt)
  | s < t = (ss, tt ++ [t] ++ [s])

run :: Game -> Game
run ([], x) = ([], x)
run (x, []) = (x, [])
run p = run $ tick p

tick2 :: Game -> Game
tick2 ([], x) = ([], x)
tick2 (x, []) = (x, [])
tick2 (s : ss, t : tt)
  | (s <= ls) && (t <= lt) && swins = (ss ++ [s] ++ [t], tt) -- s wins recursive game
  | (s <= ls) && (t <= lt)         = (ss, tt ++ [t] ++ [s]) -- t wins recursive game
  | s > t = (ss ++ [s] ++ [t], tt) -- s wins standard round
  | s < t = (ss, tt ++ [t] ++ [s]) -- t wins standard round
  | otherwise = error "SHOULD NoT BE POSSIBLE TO HIT"
  where ls = length ss
        lt = length tt
        subgame = run2 ((take s ss, take t tt), [])
        swins = not $ null $ fst $ fst subgame

run2 :: (Game, [Game]) -> (Game, [Game])
run2 (([], x), _) = (([], x), [])
run2 ((x, []), _) = ((x, []), [])
run2 (gm, prevs)
  | gm `elem` prevs = (([1], []), []) -- if we have seen this state before player 1 wins
  | otherwise      = run2 (tick2 gm, prevs ++ [gm])

score :: [Int] -> Int
score is = sum $ zipWith (*) (reverse is) [1,2..]

main :: IO ()
main = do
  putStrLn "Day 22"
  f <- readFile "../input/input22.txt"
  let q = parseInput $ lines f
  let r = run (q!!0, q!!1)
  let winner = uncurry (if' (not $ null $ fst r)) r

  let r2 = run2 ((q!!0, q!!1), [])
  let winner2 = uncurry (if' (not $ null $ fst $ fst r2)) $ fst r2

  printSoln 1 $ score winner
  printSoln 2 $ score winner2
