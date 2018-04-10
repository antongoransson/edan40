module Class1
    ( hasDups,
    removeDups,
    shortestAndLongest,
    isPermutation
    ) where

import Data.List

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


hasDups :: Eq a => [a] -> Bool
hasDups [] = False
hasDups (x:xs)
  | elem x xs       = True
  | otherwise       = hasDups(xs)

removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (x:xs) = x : removeDups [e | e <- xs, e /= x ]
data Month = Jan | Feb | March | April | May | June | July | Aug | Sep | Oct | Nov | Dec
    deriving (Read, Show,Eq)

data Date = Date Integer Month Integer

daysInMonth :: Month -> Integer -> Integer
daysInMonth m y
  | m == Jan || m == March || m == May || m == July || m == Aug || m == Oct || m == Dec  = 31
  | m == April || m == June || m == Sep || m == Nov   = 30
  | m == Feb && mod y 4 == 0  = 29
  | m == Feb = 28
  | otherwise     = 0

validDate :: Date -> Bool
validDate (Date year month day) = day <= daysInMonth month year  && day >=0


pythagoreanTriads :: Integer -> [(Integer, Integer , Integer)]
pythagoreanTriads n = [(a, b, a^2 + b^2) | a <- [1..n], b <- [a..n], a^2 + b^2 <= n]


isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] [x] = False
isPermutation [x] [] = False
isPermutation (x:xs) (ns) =  isPermutation xs $ delete x ns

getLongest:: [[Char]] -> [Char]-> [Char]
getLongest [] m = m
getLongest (s: strings) m
  | length(s) > length(m) = getLongest strings s
  | otherwise             = getLongest strings m

getShortest:: [[Char]] -> [Char]-> [Char]
getShortest [] m = m
getShortest (s: strings) m
  | length(s) < length(m)      = getShortest strings s
  | otherwise  = getShortest strings m

shortestAndLongest:: [[Char]] -> ([Char], [Char])
shortestAndLongest [] = ([],[])
shortestAndLongest (s: strings) = (getShortest strings s, getLongest strings s)
mystery xs = foldr (++) [] (map (\y -> [y]) xs)
