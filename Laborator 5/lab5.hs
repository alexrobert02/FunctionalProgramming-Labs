-- Exercitiul 0.3
data Nat = Cons [Bool] deriving Show


-- Exercitiul 0.4

instance Eq Nat where
 (==) (Cons xs) (Cons ys) = (==) xs ys

instance Ord Nat where
    compare (Cons []) (Cons []) = EQ
    compare (Cons []) _         = LT
    compare _         (Cons []) = GT
    compare (Cons (x:xs)) (Cons (y:ys))
        | length xs < length ys = LT
        | length xs > length ys = GT
        | otherwise             = case compare xs ys of
                                    EQ -> compare x y
                                    otherwise -> otherwise

nat1 = Cons [True, False, True]
nat2 = Cons [True, True, True]

result04a = nat1 == nat2         -- Output: False
result04b = nat1 `compare` nat2  -- Output: LT


-- Exercitiul 0.5

data Complex a = Complex a a deriving (Show)

instance (Floating a) => Num (Complex a) where
    (Complex a b) + (Complex c d) = Complex (a + c) (b + d)
    (Complex a b) - (Complex c d) = Complex (a - c) (b - d)
    (Complex a b) * (Complex c d) = Complex (a*c - b*d) (a*d + b*c)
    abs (Complex a b) = Complex (sqrt (a*a + b*b)) 0
    signum (Complex a b) = Complex (a/r) (b/r) where r = sqrt (a*a + b*b)
    fromInteger n = Complex (fromInteger n) 0

complex1 = Complex 2 3
complex2 = Complex (-1) 4

result05a = complex1 + complex2  -- Output: Complex 1.0 7.0
result05b = complex1 - complex2  -- Output: Complex 3.0 (-1.0)
result05c = abs complex1         -- Output: 3.605551275463989 0.0
result05d = signum complex2      -- Output: Complex (-0.24253562503633297) 0.9701425001453319


-- Exercitiul 0.6

class MyOrd a where
  myCompare :: a -> a -> Ordering
  
instance MyOrd Int where
  myCompare x y = compare x y
  
instance (MyOrd a) => MyOrd [a] where
  myCompare [] [] = EQ
  myCompare [] _ = LT
  myCompare _ [] = GT
  myCompare (x:xs) (y:ys) =
    case myCompare x y of
      EQ -> myCompare xs ys
      other -> other

int1 :: Int
int1 = 8
int2 :: Int
int2 = 3

result06 = myCompare int1 int2  -- Output: GT
