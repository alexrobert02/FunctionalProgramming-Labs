import Data.Char


-- Problema 1

char2Integer :: Char -> Integer
char2Integer c = (fromIntegral (ord c)) - 48

string2Integer :: String -> Integer
string2Integer [] = 0
string2Integer (s:sx) = char2Integer s * 10^(length sx) + string2Integer sx

result1a = char2Integer '5'        -- Output: 5
result1b = string2Integer "12345"  -- Output: 12345


-- Problema 2

countCapitals :: String -> Int
countCapitals [] = 0
countCapitals (s:sx)
    | s == toUpper s = 1 + countCapitals sx
    | otherwise = countCapitals sx

result2 = countCapitals "Haskell is fun"  -- Output: 3


-- Problema 3

data Vehicle = Car {brand :: String, model :: String, year :: Int}
             | Ship {brand :: String, year :: Int}
             | Bicycle {brand :: String, model :: String}
             | Truck {brand :: String, model :: String, tonage :: Float}
             deriving Show

carExample = Car "Mertan" "S-Class" 2023
shipExample = Ship "Queen Mary" 2005
bikeExample = Bicycle "Trek" "Mountain Bike"
truckExample = Truck "Volvo" "FH16" 25.0

vehicleBrand :: Vehicle -> String
vehicleBrand (Car brand _ _) = brand
vehicleBrand (Ship brand _) = brand
vehicleBrand (Bicycle brand _) = brand
vehicleBrand (Truck brand _ _) = brand

result3a = vehicleBrand carExample    -- Output: "Mertan"
result3b = vehicleBrand shipExample   -- Output: "Queen Mary"
result3c = vehicleBrand bikeExample   -- Output: "Trek"
result3d = vehicleBrand truckExample  -- Output: "Volvo"


-- Problema 4

data Expr = Const Int
          | Var String
          | Add Expr Expr
          | Mult Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          deriving Show

result4 = Sub (Add (Mult (Var "x") (Const 7)) (Const 10)) (Const 23)


-- Problema 5

countDigits :: String -> Int
countDigits str = length (filter isDigit' str)
    where 
        isDigit' c = c >= '0' && c <= '9'

result5 = countDigits "Hello123World456"  -- Output: 6


-- Problema 6

appFOverList :: [a] -> (a -> [a]) -> [a]
appFOverList list f = concat (map f list)

decrement :: Int -> [Int]
decrement x = [x - 1]

result6a = appFOverList [10,100,0,-10] decrement           -- Output: [9,99,-1,-11]
result6b = appFOverList [10,100] (\x -> [ x - 1, x + 1 ])  -- Output: [9,11,99,101]
result6c = appFOverList [] decrement                       -- Output: []
