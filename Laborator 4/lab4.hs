-- Exercitiul 1.1

addThree :: (Int, Int, Int) -> Int
addThree (x,y,z) = x + y + z

addThree' :: Int -> Int -> Int -> Int
addThree' x y z = x + y + z

result11 = addThree' 1 2 3  -- Output: 6


-- Exercitiul 2.1

applyFuncAndSum :: (Int -> Int) -> Int -> Int -> Int
applyFuncAndSum f a b = sum [f x | x <- [a..b]]

square :: Int -> Int
square x = x * x

result21 = applyFuncAndSum square 1 5  -- Output: 55
-- square 1 + square 2 + square 3 + square 4 + square 5 = 1 + 4 + 9 + 16 + 25 = 55


-- Exercitiul 2.2

compunere :: (b -> c) -> (a -> b) -> a -> c
compunere f g x = f (g x)

addOne :: Int -> Int
addOne x = x + 1

addOneThenSquare :: Int -> Int
addOneThenSquare = compunere square addOne

result22 = addOneThenSquare 3  -- Output: 16
-- (3 + 1) * (3 + 1) = 16


-- Exercitiul 2.3

compunereLista :: [a -> a] -> a -> a
compunereLista [] x = x
compunereLista (f:fs) x = compunereLista fs (f x)

fs :: [Int -> Int]
fs = [(*2), (+1), (^2)]

compus :: Int -> Int
compus = compunereLista fs

result23 = compus 3  -- Output: 49
--((3 * 2) + 1) ^2 = 49


-- Exercitiul 2.4

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

result24 = sumList [1, 2, 3, 4]  -- Output: 10
-- 1 + 2 + 3 + 4 = 10


-- Exercitiul 2.5

mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (x:xs) = f x : mapList f xs

result25 = mapList length ["apple", "banana", "cherry"]  -- Output: [5, 6, 6]


-- Exercitiul 2.6

filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList p (x:xs)
  | p x       = x : filterList p xs
  | otherwise = filterList p xs

seImparteLaDoi :: Int -> Bool
seImparteLaDoi x | x `mod` 2 == 0 = True
                 | otherwise = False

result26 = filterList seImparteLaDoi [1, 2, 3, 4, 5, 6, 7, 8]  -- Output: [2, 4, 6, 8]


-- Exercitiul 2.7

myfoldr = foldr:: (a -> b -> b) -> b -> [a] -> b
foldlList :: (b -> a -> b) -> b -> [a] -> b
foldlList _ acc [] = acc
foldlList f acc (x:xs) = foldlList f (f acc x) xs

foldrList :: (a -> b -> b) -> b -> [a] -> b
foldrList _ acc [] = acc
foldrList f acc (x:xs) = f x (foldrList f acc xs)

result27a = foldlList (+) 3 [1,2,3,4,5]
result27b = foldrList (+) 8 [2,3,4,5,6]


-- Exercitiul 2.8

data Arb = Frunza Integer | Nod Integer Arb Arb deriving (Show, Eq)
preordine :: Arb -> (Integer -> b) -> [b]
preordine (Frunza x) f = [f x]
preordine (Nod x stanga dreapta) f = f x : preordine stanga f ++ preordine dreapta f

postordine :: Arb -> (Integer -> b) -> [b]
postordine (Frunza x) f = [f x]
postordine (Nod x stanga dreapta) f = postordine stanga f ++ postordine dreapta f ++ [f x]

inordine :: Arb -> (Integer -> b) -> [b]
inordine (Frunza x) f = [f x]
inordine (Nod x stanga dreapta) f = inordine stanga f ++ [f x] ++ inordine dreapta f

myArb :: Arb
myArb = Nod 4 (Nod 2 (Frunza 1) (Frunza 3)) (Nod 6 (Frunza 5) (Frunza 7))

addTwo :: Integer -> Integer
addTwo x = x + 2

result28a = preordine myArb addTwo
result28b = postordine myArb addTwo
result28c = inordine myArb addTwo


-- Exercitiul 2.9

parcurgere :: Arb -> (Integer -> b) -> (Arb -> (Integer -> b) -> [b]) -> [b]
parcurgere arb f parcurgereStrategie = parcurgereStrategie myArb f

result29 = parcurgere myArb addTwo postordine
