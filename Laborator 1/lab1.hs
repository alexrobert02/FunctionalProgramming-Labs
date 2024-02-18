-- Exercitiul 3.10

maxim x y z | x > y && x > z = x
maxim x y z | y > x && y > z = y
maxim x y z | z > x && z > y = z

result310 = maxim 2 5 4  -- Output: 5


-- Exercitiul 3.12

fib x = if x <= 1 then x else fib (x-1) + fib (x-2)

fib' x | x <=1 = x
fib' x = fib' (x-1) + fib' (x-2)

result312 = fib' 8  -- Output: 21


-- Exercitiul 3.13

cmmdc x y = if x /= y then (if x > y then cmmdc (x-y) y else cmmdc x (y-x)) else x

cmmdc' :: Int -> Int -> Int
cmmdc' x y | x == y = x
cmmdc' x y | x > y = cmmdc' (x-y) y
cmmdc' x y = cmmdc' x (y-x) 

result313 = cmmdc' 25 15  -- Output: 5
