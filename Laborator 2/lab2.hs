-- Exercitiul 1

and' :: Bool -> Bool -> Bool
and' False _ = False
and' _ False = False
and' _ _ = True

neg :: Bool -> Bool
neg True = False
neg False = True

nand :: Bool -> Bool -> Bool
nand False _ = True
nand _ False = True
nand _ _ = False

nand' :: Bool -> Bool -> Bool
nand' b1 b2 = neg (and' b1 b2)


-- Exercitiul 2

hasDivisors :: Integer -> Integer -> Integer -> Bool
hasDivisors n a b | a > b = False
hasDivisors n a b | mod n a == 0 = True 
hasDivisors n a b = hasDivisors n (a+1) b

isPrime :: Integer -> Bool
isPrime n = neg (hasDivisors n 2 (n-1))

result2 = isPrime 11  -- Output: True


-- Exercitiul 3

cmmdc :: Int -> Int -> Int
cmmdc x y | x == y = x
cmmdc x y | x > y = cmmdc (x-y) y
cmmdc x y = cmmdc x (y-x)

cmmdc' :: Int -> Int -> Int
cmmdc' x y | y == 0 = x
cmmdc' x y = cmmdc' y (mod x y)

result3a = cmmdc 4 12   -- Output: 4
result3b = cmmdc' 4 12  -- Output: 4


-- Exercitiul 5

fibo :: Integer -> Integer
fibo x | x <=1 = x
fibo x = fibo (x-1) + fibo (x-2)

fiboaux :: Integer -> Integer -> Integer -> Integer
fiboaux 0 a _ = a
fiboaux n a b = fiboaux (n-1) b (a+b)  -- a si b sunt doua numere Fibonacci consecutive

fibo' :: Integer -> Integer
fibo' n = fiboaux n 0 1

result5a = fibo 7   -- Output: 13
result5b = fibo' 7  -- Output: 13
