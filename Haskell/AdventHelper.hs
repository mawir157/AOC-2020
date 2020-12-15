module AdventHelper where
import Data.List
import Data.List.Split

splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

zipWithFn :: (a -> b) -> [a] -> [(a,b)]
zipWithFn fn as  = zip as (map (fn) as)

printSoln :: (Show a) => Integer -> a -> IO()
printSoln n s = do
  putStrLn ("  Part " ++ (show n) ++ ": " ++ (show s))

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

concat' :: [[a]] -> [a] -> [a]
concat' [] _ = []
concat' [s] _ = s
concat' (s:ss) c  = s ++ c ++ concat' ss c

parseLineGroups :: String -> [String] -> [String]
parseLineGroups _ [] = []
parseLineGroups c ss = [(concat' b c)] ++ (parseLineGroups c ss')
  where b = takeWhile (\l -> length l > 0) ss
        ss' = drop 1 $ dropWhile (\l -> length l > 0) ss


diff :: [Integer] -> [Integer]
diff [] = error "Diff of single element is nonsensical"
diff [_] = []
diff (x:y:xs) = [(x-y)] ++ diff (y:xs)

minPair :: (Ord a) => [(a,b)] -> (a,b)
minPair [] = error "Empty List"
minPair [x] = x
minPair (x:y:xs) = if' (fst x < fst y) (minPair (x:xs)) (minPair (y:xs))

chiRemThm :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
chiRemThm (a1, p1) (a2, p2) = (a3, p1 * p2)
  where a3 = head $ filter (\x -> x `mod` p2 == a2 `mod` p2) cands
        cands = [ a1 + n * p1 | n <- [1..p2]]
