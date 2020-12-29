import AdventHelper

maxMod :: Int -> [Int] -> Int
maxMod x xs
  | x `elem` xs = x
  | otherwise   = maxMod ((x-1) `mod` 10) xs

-- e.g. rotateAt 5 [1,2,3,4,5,6,7,8,9] = [5,6,7,8,9,1,2,3,4]
rotateAt :: (Eq a) => a -> [a] -> [a]
rotateAt x xs = dropWhile (/= x) xs ++ takeWhile (/= x) xs

-- e.g. rotateAt 5 [1,2,3,4,5,6,7,8,9] = [6,7,8,9,1,2,3,4,5]
rotateAfter :: (Eq a) => a -> [a] -> [a]
rotateAfter x xs = drop 1 $ dropWhile (/= x) xs ++ takeWhile (/= x) xs ++ [x]

-- we always rotate so that the 0th element is the current
tick :: [Int] -> [Int]
tick v = rotateAfter cur w
  where cur = head v
        remaining = cur : drop 4 v
        v' = rotateAt (maxMod (cur-1) remaining) remaining
        w = [head v'] ++ take 3 (drop 1 v) ++ tail v'

run :: Int -> [Int] -> [Int]
run 0 xs = xs
run n xs = run (n-1) $ tick xs

main :: IO ()
main = do
  putStrLn "Day 23"
  -- let s = [3,8,9,1,2,5,4,6,7]
  let s = [5,2,3,7,6,4,8,1,9]
  let s1 = init $ rotateAfter 1 $ run 100 s
  let p1 = read (concatMap show s1) :: Integer

  printSoln 1 p1
  printSoln 2 $ 2
