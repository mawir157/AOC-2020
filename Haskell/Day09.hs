import AdventHelper

block :: Int -> [Integer] -> Int -> ([Integer], Integer)
block size is offset = (take size $ drop offset is,
                        head $ drop (offset+ size) is)

test :: ([Integer], Integer) -> Bool
test ([_], _) = False
test ((x:xs), n)
  | elem (n - x) xs = True
  | otherwise       = test (xs, n)

findRange :: [Integer] -> Integer -> Int -> [Integer]
findRange xs target n
  | s == target = xs'
  | s >  target = findRange (drop 1 xs) target 2
  | s <  target = findRange xs target (succ n)
  where xs' = take n xs
        s = sum xs'


main = do
  putStrLn "Day 9"
  f <- readFile "../input/input09.txt"
  let ns = map (read) $ lines f :: [Integer]
  let size = 117

  let q = head $ filter (not . test . block size ns) [0,1..]
  let r = findRange ns (ns!!(q+size)) 2

  printSoln 1 (ns!!(q+size))
  printSoln 2 (maximum r + minimum r)
