import AdventHelper

import qualified Data.Bits as B
import Data.List
import Data.List.Split
import qualified Data.Map as Map

type Memory = Map.Map Integer Integer
type Mask = String

type Machine = (Mask, Memory)
------------------------------- Part 1 -----------------------------------------
tick :: Machine -> String -> Machine
tick (msk, mem) s
  | head ss == "mask" = (reverse $ last ss, mem)
  | otherwise         = writeToMem (msk, mem) (head ss) (ss!!1)
  where ss = splitOn " = " s

applyMask :: Integer ->  Int -> String -> Integer
applyMask x _ [] = x
applyMask x i (s:ss)
  | s == 'X' = applyMask x (i+1) ss
  | s == '0' = applyMask (B.clearBit x i) (i+1) ss
  | s == '1' = applyMask (B.setBit x i) (i+1) ss

writeToMem :: Machine -> String -> String -> Machine
writeToMem (mask, mem) lhs rhs = (mask, Map.insert reg val' mem)
  where reg = read (takeWhile (/= ']') $ drop 4 lhs) :: Integer
        val = read rhs :: Integer
        val' = applyMask val 0 mask
------------------------------- Part 2 -----------------------------------------
applyMaskMem :: [Integer] -> Int -> String -> [Integer]
applyMaskMem xs _ [] = xs
applyMaskMem xs i (s:ss)
  | s == 'X' = applyMaskMem xsX (i+1) ss
  | s == '0' = applyMaskMem xs (i+1) ss
  | s == '1' = applyMaskMem xs1 (i+1) ss
  where xs1 = map (`B.setBit` i) xs
        xsX = concatMap (\t -> [B.setBit t i, B.clearBit t i]) xs

writeToMem2 :: Machine -> String -> String -> Machine
writeToMem2 (mask, mem) lhs rhs = (mask, mem')
  where reg = read (takeWhile (/= ']') $ drop 4 lhs) :: Integer
        val = read rhs :: Integer
        regs = applyMaskMem [reg] 0 mask
        mem' = foldl' (\m r -> Map.insert r val m) mem regs 

tick2 :: Machine -> String -> Machine
tick2 (msk, mem) s
  | head ss == "mask" = (reverse $ last ss, mem)
  | otherwise         = writeToMem2 (msk, mem) (head ss) (ss!!1)
  where ss = splitOn " = " s

main = do
  putStrLn "Day 14"
  f <- readFile "../input/input14.txt"
  let l = lines f 

  let skynet = ("", Map.empty) :: Machine
  let (_, mem1) = foldl' tick skynet l
  printSoln 1 $ sum $ Map.elems mem1

  let holly = ("", Map.empty) :: Machine
  let (_, mem2) = foldl' tick2 holly l
  printSoln 2 $ sum $ Map.elems mem2
