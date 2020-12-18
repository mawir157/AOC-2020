import AdventHelper

import Data.List.Split
import Data.List.Utils

import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

-- Reduces an expression with no brakcets, resolve left to right
removeLR :: String -> String
removeLR ss
  | ss == ss' = ss
  | otherwise = removeLR ss'
  where ss' = reduceLR ss

reduceLR :: String -> String
reduceLR ss
  | '+' `elem` ss || '*' `elem` ss = reduceLR (nw ++ tt)
  | otherwise                       = ss
  where v1  = readInt ss
        ss' = dropWhile (\c -> c /= '*' && c /= '+') ss
        fn = head ss'
        v2 = readInt $ tail ss'
        tt = dropWhile (\c -> c /= '*' && c /= '+') $ tail ss'
        nw = if' (fn == '*') (show (v1 * v2)) (show (v1 + v2))

-- reduces an expression resolving addition before multiplication
removePM :: String -> String
removePM ss
  | '+' `elem` ss = removePM $ reducePlus ss
  | '*' `elem` ss = removePM $ reduceMult ss
  | otherwise     = ss

reducePlus :: String -> String
reducePlus ss
  | '+' `elem` ss = reducePlus $ replaceFirst pl nw ss
  | otherwise     = ss
  where pl = ss =~ "[0-9]+\\+[0-9]+" :: String
        [v1,v2] = map read $ splitOn "+" pl :: [Integer]
        nw = show (v1 + v2)

reduceMult :: String -> String
reduceMult ss
  | '*' `elem` ss = reduceMult $ replaceFirst pl nw ss
  | otherwise     = ss
  where pl = ss =~ "[0-9]+\\*[0-9]+" :: String
        [v1,v2] = map read $ splitOn "*" pl :: [Integer]
        nw = show (v1 * v2)

-- removes a bracket without any nested brackets inside
removeBra :: Bool -> String -> String
removeBra lr ss
  | not (null bra) = replace bra new ss 
  | otherwise        = ss
  where bra = ss =~ "\\([0-9+*]+\\)" :: String 
        bra' = init $ tail bra
        new = if' lr (removeLR bra') (removePM bra') 

reduce :: Bool-> String -> String
reduce lr ss
  | '(' `elem` ss                  = reduce lr bra
  | '+' `elem` ss || '*' `elem` ss = reduce lr ext
  | otherwise                      = ss
  where bra = removeBra lr ss
        ext = if' lr (removeLR ss) (removePM ss)

main = do
  putStrLn "Day 18"
  f <- readFile "../input/input18.txt"
  let exps = map (filter (/= ' ')) $ lines f

  let values = map (read . reduce True) exps :: [Integer]
  printSoln 1 $ sum values

  let values2 = map (read . reduce False) exps :: [Integer]
  printSoln 2 $ sum values2
