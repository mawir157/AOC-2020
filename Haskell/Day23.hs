import AdventHelper

-- import qualified Data.Map as Map
-- import qualified Data.Bimap as Bimap
import Data.List
import qualified Data.Sequence as Seq
import Data.Maybe

import Debug.Trace

type Game = Seq.Seq Int

-- maxMod :: Int -> [Int] -> Int
-- maxMod x xs
--   | x `elem` xs = x
--   | otherwise   = maxMod ((x-1) `mod` 10) xs

-- -- e.g. rotateAt 5 [1,2,3,4,5,6,7,8,9] = [5,6,7,8,9,1,2,3,4]
-- rotateAt :: (Eq a) => a -> [a] -> [a]
-- rotateAt x xs = dropWhile (/= x) xs ++ takeWhile (/= x) xs

-- -- e.g. rotateAt 5 [1,2,3,4,5,6,7,8,9] = [6,7,8,9,1,2,3,4,5]
-- rotateAfter :: (Eq a) => a -> [a] -> [a]
-- rotateAfter x xs = drop 1 $ dropWhile (/= x) xs ++ takeWhile (/= x) xs ++ [x]

-- -- we always rotate so that the 0th element is the current
-- tick :: [Int] -> [Int]
-- tick v = rotateAfter cur w
--   where cur = head v
--         remaining = cur : drop 4 v
--         v' = rotateAt (maxMod (cur-1) remaining) remaining
--         w = [head v'] ++ take 3 (drop 1 v) ++ tail v'

-- run :: Int -> [Int] -> [Int]
-- run 0 xs = xs
-- run n xs = run (n-1) $ tick xs

buildGame :: Int -> [Int] -> Game
buildGame size input = Seq.fromList $ input ++ extra
  where extra = [(length input + 1), (length input + 2)..size]

seqMaxMod :: Int -> Int -> [Int] -> Int
seqMaxMod m x bad
  | x == 0       = seqMaxMod m m bad
  | x `elem` bad = seqMaxMod m (x-1) bad
  | otherwise    = x

seqRotateAfter :: (Eq a) => a -> Seq.Seq a -> Seq.Seq a
seqRotateAfter x xs = Seq.drop 1 $ Seq.dropWhileL (/= x) xs Seq.><
                      Seq.takeWhileL (/= x) xs Seq.>< Seq.singleton x

parseToString :: (Show a) => Seq.Seq a -> String
parseToString Seq.Empty = []
parseToString (s Seq.:<| ss) = (show s) ++ parseToString ss

grabBad :: [Int] -> Game -> [Int]
grabBad is v = map (\i -> fromJust $ Seq.lookup i v) is

deleteList :: [Int] -> Game -> Game
deleteList xs g = foldl' (\s x -> Seq.deleteAt x s) g xs

insertInto :: Int -> [Int] -> Game -> Game
insertInto n xs g = foldl' (\s x -> Seq.insertAt n x s)  g xs

seqTick :: Int -> (Game, Int) -> (Game, Int)
seqTick m (v, i) = (w, (i' + 1) `mod` m)
  where nextIs = [mod (i+1) m, mod (i+2) m, mod (i+3) m]
        curV = fromJust $ Seq.lookup i v
        moveV = grabBad nextIs v
        destV = seqMaxMod m (curV-1) moveV
        v' = deleteList (reverse $ sort nextIs) v
        destI = fromJust $ Seq.findIndexL (== destV) v'
        w = insertInto (destI + 1 `mod` m) (reverse moveV) v'
        i' = fromJust $ Seq.findIndexL (== curV) w

seqRun :: Int -> Int -> (Game, Int) -> (Game, Int)
seqRun 0 _ xs = xs
seqRun n m xs
  | Seq.length (fst xs') == 0 = error "Spurious pattern match"
  | otherwise                   = seqRun debug m xs'
  where xs' = seqTick m xs
        debug = traceShowId (n - 1)

part2 :: Game -> Int
part2 g = val1 * val2
  where m = Seq.length g
        oneI = fromJust $ Seq.findIndexL (== 1) g
        val1 = fromJust $ Seq.lookup ((oneI+1) `mod` m) g
        val2 = fromJust $ Seq.lookup ((oneI+2) `mod` m) g

main :: IO ()
main = do
  putStrLn "Day 23"
  let s = [3,8,9,1,2,5,4,6,7]
  -- let s = [5,2,3,7,6,4,8,1,9]

  let s1 = buildGame 9 s
  let k = seqRun 100 (Seq.length s1) (s1, 0)
  let p1 = read (parseToString $ seqRotateAfter 1 $ fst k) ::Integer
  printSoln 1 p1

  -- let s2 = buildGame 10 s
  -- let k2 = seqRun 10000000 (Seq.length s2) (s2, 0)
  -- let p2 = part2 $ fst k2
  -- printSoln 2 p2
