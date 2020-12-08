import AdventHelper

import Data.List.Split

data Ins = ACC | JMP | NOP deriving (Eq, Show)
data Code = CONT | HALT | LOOP | ERR deriving (Eq, Show)

type Com = (Ins, Int)
-- accumalator, instruction pointer, program, history, status code
type Machine = (Int, Int, [Com], [Int], Code) 

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
tick (acc, ptr, prg, his, code)
  | elem ptr his = (acc, ptr, prg, his, LOOP)
  | ptr >= length prg || ptr < 0 = (acc, ptr, prg, his, HALT)
  | c == ACC  = (acc + v, succ ptr, prg, his ++ [ptr], CONT)
  | c == JMP  = (acc,     ptr + v,  prg, his ++ [ptr], CONT)
  | c == NOP  = (acc,     succ ptr, prg, his ++ [ptr], CONT)
  | otherwise = (acc, ptr, prg, his, ERR)
  where (c,v) = (prg!!ptr)

tickUntilRep :: Machine -> Machine
tickUntilRep (acc, ptr, prg, his, code)
  | code == CONT = tickUntilRep $ tick (acc, ptr, prg, his, code)
  | otherwise    = (acc, ptr, prg, his, code)

fixProgram :: [Com] -> Int -> [Com]
fixProgram cs n = (take n cs) ++ [(c',v)] ++ (drop (n+1) cs)
  where (c,v) = cs!!n
        c' = if' (c == ACC) (ACC) (if' (c == JMP) (NOP) (JMP))

main = do
  putStrLn "Day 8"
  f <- readFile "../input/input08.txt"
  let s = map (parseInput) $ lines f

  let (acc,_,_,_,_) = tickUntilRep (0,0,s,[],CONT)
  printSoln 1 acc

  let fixed = map (fixProgram s) [0..((length s) - 1)]
  let fs = map (\x -> tickUntilRep (0,0,x,[],CONT)) fixed
  let (acc',_,_,_,_) = head $ dropWhile (\(_,_,_,_,c) -> c /= HALT) fs
  printSoln 2 acc'
