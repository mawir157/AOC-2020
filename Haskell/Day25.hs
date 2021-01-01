import AdventHelper

-- b^a mod p
modPow :: Integer -> Integer -> Integer -> Integer
modPow b 0 p = 1
modPow b 1 p = mod b p
modPow b a p | even a = mod ((modPow b (div a 2) p) ^ 2) p
             | odd  a = mod ((modPow b (div (a-1) 2) p) ^ 2 * b) p

-- a*b mod p
modMult :: Integer -> Integer -> Integer -> Integer
modMult p a b = mod (a * b) p

-- b^a = x mod p <-- find a
discreteLog :: Integer -> Integer -> Integer -> Integer
discreteLog b x p = 1 + fromIntegral (length $ takeWhile (/= x) $ scanl1 (modMult p) (repeat b))

main :: IO ()
main = do
  putStrLn "Day 25"
  f <- readFile "../input/input25.txt"
  let [cardPub, doorPub] = map read (lines f) :: [Integer]

  let doorLoop = discreteLog 7 doorPub 20201227

  printSoln 1 $ modPow cardPub doorLoop 20201227
  printSoln 2 "See you next year, I love you."
