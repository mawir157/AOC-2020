import AdventHelper

import Data.List.Split

data Ins = ACC | JMP | NOP  deriving (Eq, Show)

type Com = (Ins, Int)
type Machine = (Int, Int, [Com]) -- accumalator, instruction pointer, program

parsePM :: String -> Int
parsePM (s:ss) = (if' (s == '-') (-1) 1) * (read ss :: Int) 

parseInput :: String -> Com
parseInput s
  | ss!! 0 == "acc" = (ACC, v)
  | ss!! 0 == "jmp" = (JMP, v)
  | ss!! 0 == "nop" = (NOP, v)
  where ss = splitOn " " s
        v = parsePM (ss!!1)

tick :: Machine -> Machine
tick (acc, ptr, prg)
  | ptr >= length prg = (acc,             ptr,              [])
  | fst com == ACC    = (acc + (snd com), succ ptr,         prg)
  | fst com == JMP    = (acc,             ptr  + (snd com), prg)
  | fst com == NOP    = (acc,             succ ptr,         prg)
  where com = (prg!!ptr)

tickUntilRep :: (Machine, [Int]) -> (Machine, [Int])
tickUntilRep ((acc,ptr,prg), seen)
  | elem ptr seen = ((acc,ptr,prg), seen)
  | otherwise     = tickUntilRep (tick (acc,ptr,prg), seen ++ [ptr])

fixProgram :: [Com] -> Int -> [Com]
fixProgram cs n = (take n cs) ++ [(c',v)] ++ (drop (n+1) cs)
  where (c,v) = cs!!n
        c' = if' (c == ACC) (ACC) (if' (c == JMP) (NOP) (JMP))

main = do
  putStrLn "Day 8"
  f <- readFile "../input/input08.txt"
  let s = map (parseInput) $ lines f

  let ((acc,_,_), _) = tickUntilRep ((0,0,s), [])
  printSoln 1 acc

  let fixed = map (fixProgram s) [0..((length s) - 1)]
  let fs = map (\x -> tickUntilRep ((0,0,x), [])) fixed
  let ((acc',_,_), _) = head $ dropWhile (\((_,_,p),_) -> p /= []) fs
  printSoln 2 acc'
