import AdventHelper

import Data.List
import Data.List.Split

import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

complete = ["byr","cid","ecl","eyr","hcl","hgt","iyr","pid"]

checkId1 :: String -> Bool
checkId1 s = length f == 0
  where s' = splitOn " " s
        cs = sort $ map (takeWhile (/= ':')) s'
        f = (complete \\ cs) \\ ["cid"]

checkId2 :: String -> Bool
checkId2 s = all (checkUnit) us
  where s' = splitOn " " s
        us = map (\x -> tuplify2 $ splitOn ":" x) s'

checkUnit :: (String, String) -> Bool
checkUnit ("byr",v) = v =~ "(19[2-9][0-9]|200[0-2])"
checkUnit ("iyr",v) = v =~ "(201[0-9]|2020)"
checkUnit ("eyr",v) = v =~ "(202[0-9]|2030)"
checkUnit ("hgt",v) = v =~ "(59in|6[0-9]in|7[0-6]in|1[5-8][0-9]cm|19[0-3]cm)"
checkUnit ("hcl",v) = v =~ "^#([0-9a-f]){6}$"
checkUnit ("ecl",v) = v =~ "(amb|blu|brn|gry|grn|hzl|oth)"
checkUnit ("pid",v) = v =~ "^([0-9]){9}$"
checkUnit ("cid",v) = True
checkUnit otherwise = error "Unidentified category"

main = do
  putStrLn "Day 4"
  f <- readFile "../input/input04.txt"
  let s = parseLineGroups " " $ lines f

  printSoln 1 (length $ filter (checkId1) s)
  printSoln 2 (length $ filter (checkId2) $ filter (checkId1) s)
