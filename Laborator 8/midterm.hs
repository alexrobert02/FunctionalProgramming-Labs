-- Problema 1

sumOfSquares :: Int -> Int
sumOfSquares n
  | n <= 0    = 0                         -- Caz de bază: dacă n este mai mic sau egal cu 0, suma este 0
  | otherwise = n*n + sumOfSquares (n-1)  -- Altfel, adăugăm pătratul lui n la suma pătratelor de la 1 la n-1

result1 = sumOfSquares 6  -- Output: 91
-- 1^2 + 2^2 + 3^2 + 4^2 + 5^2 + 6^2 = 1 + 4 + 9 + 16 + 25 + 36 = 91


-- Problema 2

countOccurrences :: Eq a => [a] -> a -> Int
countOccurrences [] _ = 0                     -- Caz de bază: lista este goală, deci nu există nicio apariție
countOccurrences (x:xs) n
  | x == n    = 1 + countOccurrences xs n     -- Dacă x este egal cu n, adăugăm 1 la rezultat și continuăm cu restul listei
  | otherwise = countOccurrences xs n         -- Altfel, ignorăm x și continuăm cu restul listei

result2 = countOccurrences [4,5,8,4,7,4,1,8] 4  -- Output: 3


-- Problema 3

longestIncreasingSubsequence :: [Int] -> Int
longestIncreasingSubsequence [] = 0              -- Caz de bază: lista este goală, lungimea subsirului este 0
longestIncreasingSubsequence [x] = 1             -- Caz de bază: lista conține un singur element, lungimea subsirului este 1
longestIncreasingSubsequence (x:xs) = maximum [1 + longestIncreasingSubsequence [y | y <- xs, y > x], longestIncreasingSubsequence xs]

result3 = longestIncreasingSubsequence [6,2,4,10,5,3,7,9]  -- Output: 5


-- Problema 4

data Vehicle
  = Car { carBrand :: String, carModel :: String, carYear :: Int }
  | Ship { shipBrand :: String, shipYear :: Int }
  | Bicycle { bikeBrand :: String, bikeModel :: String }
  | Truck { truckBrand :: String, truckModel :: String, truckTonage :: Int } 
  deriving Show

carExample :: Vehicle
carExample = Car "Ford" "Mustang" 2021

shipExample :: Vehicle
shipExample = Ship "Titanic" 1912

bikeExample :: Vehicle
bikeExample = Bicycle "Giant" "Escape 3"

truckExample :: Vehicle
truckExample = Truck "Volvo" "FH16" 40


-- Problema 5

data Expr
  = Const Int                 -- Constantă numerică
  | Var String                -- Variabilă
  | Add Expr Expr             -- Adunare
  | Sub Expr Expr             -- Scădere
  | Mul Expr Expr             -- Înmulțire
  | Div Expr Expr             -- Împărțire
  deriving Show

-- (x * 7 + 10) - 23
exampleExpr :: Expr
exampleExpr = Sub (Add (Mul (Var "x") (Const 7)) (Const 10)) (Const 23)


-- Problema 6

data Shape
  = Circle Double                  -- Cerc cu raza
  | Rectangle Double Double        -- Dreptunghi cu latime si inaltime
  | Triangle Double Double Double  -- Triunghi cu laturile

area :: Shape -> Double
area (Circle r) = pi * r * r                 -- Aria cercului: pi * r^2
area (Rectangle w h) = w * h                 -- Aria dreptunghiului: latime * inaltime
area (Triangle a b c) =
  let s = (a + b + c) / 2                    -- Semiperimetrul triunghiului
  in sqrt (s * (s - a) * (s - b) * (s - c))  -- Formula lui Heron pentru aria triunghiului

circleExample :: Shape
circleExample = Circle 5.0

rectangleExample :: Shape
rectangleExample = Rectangle 3.0 4.0

triangleExample :: Shape
triangleExample = Triangle 3.0 4.0 5.0

circleArea = area circleExample        -- Output: 78.53981633974483
rectangleArea = area rectangleExample  -- Output: 12.0
triangleArea = area triangleExample    -- Output: 6.0


-- Problema 7

sumOfEvenSquares :: [Int] -> Int
sumOfEvenSquares xs =
  let evenSquares = map (^2) (filter even xs)  -- Obținem lista pătratelor numerelor pare
  in foldl (+) 0 evenSquares                   -- Calculăm suma elementelor din lista

result7 = sumOfEvenSquares [1,2,3,4,5,6]  -- Output: 56
-- 2^2 + 4^2 + 6^2 = 4 + 16 + 36 = 56


-- Problema 8

averageGrade :: [(String, [Float])] -> [(String, Float)]
averageGrade = map (\(name, grades) -> (name, foldl (+) 0 grades / fromIntegral (length grades)))


students = [("John", [8.5, 9.0, 7.5]), 
            ("Alice", [7.0, 6.5, 8.0]), 
            ("Bob", [6.0, 7.0, 6.5])]

result8 = averageGrade students  -- Output: [("John",8.333333),("Alice",7.1666665),("Bob",6.5)]


-- Problema 9

myhead :: [a] -> Maybe a
myhead = foldr (\x _ -> Just x) Nothing

result9 = myhead [4,2,8,7]  -- Output: 4
