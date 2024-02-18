-- Exercitiul 1

qs :: Ord a => [a] -> [a]
qs [] = []
qs (hd:tl) = qs (filter (<=hd) tl) ++ [hd] ++ qs (filter (>hd) tl)

minList :: Ord a => [a] -> a
minList xs = head (qs xs)

result1 = minList [3,6,4,8,2,5]


-- Exercitiul 4

maxList :: Ord a => [a] -> a
maxList [x] = x
maxList (x:y:xs) = if x > y then x else maxList(y:xs)

result4 = maxList [3,6,4,8,2,5]


-- Exercitiul 5

fib :: [Int]
fib = 0 : 1 : sieve fib
  where
    sieve (x:y:xs) = (x + y) : sieve (y:xs)

result5 = take 15 fib


-- Exercitiul 6

primeBools :: [Bool]
primeBools = map isPrime [2..]

result6 = take 15 primeBools


-- Exercitiul 7

isPrime :: Int -> Bool
isPrime n = n > 1 && all (\x -> n `mod` x /= 0) [x | x <- [2..n-1], x*x <= n]

primes :: [Int]
primes = 2 : filter isPrime [3,5..]

result7 = take 15 primes
