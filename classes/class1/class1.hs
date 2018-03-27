module Main where

main = putStrLn "Hello, world!"

maxi x y
  | x <= y    = y
  | otherwise = x

sumsq1 n = sqr[1..n]
  where
    sqr [] = 0
    sqr (n:ns) = n^2 + (sqr(ns))

sumsq2 n = foldl (+) 0 (map(^2) [1..n])

-- hanoi
hanoi n
  | n <= 0  = 0
  | otherwise =  2^(n -1) + hanoi(n-1)


smallestFactor n = nextFactor 1 n

nextFactor k n
  | k == n                = k
  | n `mod`  (k + 1) == 0 = k + 1
  | otherwise             = nextFactor (k + 1)  n

numFactors n
