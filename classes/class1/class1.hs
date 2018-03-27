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

numFactors n = length( factorials [1..n])
   where
      factorials (x:xs)
        | x >= n      = n : xs
        | otherwise   = x : factorials [ nextFactor x n]


multiply1 :: Num a => [a] -> a

multiply1 [] = 1
multiply1 (n:ns) = n * multiply1(ns)

substitute x y string = map(\n -> if n == x then y else n) string


duplicates :: Eq a => [a] -> Bool
duplicates (x:xs)
  | length xs == 0  = False
  | elem x xs       = True
  | otherwise       = duplicates(xs)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates ns = sieve ns []
   where
      sieve (n:ns) xs
        | length ns == 0  = xs
        | not (elem n xs) = sieve ns (xs ++ [n])
        | otherwise  = sieve ns xs
