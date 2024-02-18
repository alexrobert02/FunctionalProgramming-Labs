-- Exercitiul 1.3

data Culoare = Rosu
             | Galben
             | Albastru
             | Verde
             | Mov
             | Gri
             | Roz
               deriving (Show)


-- Exercitiul 1.4

data MobileDevice = Smartphone Culoare
                  | Laptop Culoare
                  | Tablet Culoare
                    deriving (Show)

printColor :: MobileDevice -> Culoare
printColor (Smartphone culoare) = culoare
printColor (Laptop culoare) = culoare
printColor (Tablet culoare) = culoare

data MobileDevice' = Smartphone' {culoare :: Culoare}
                   | Laptop' {culoare :: Culoare}
                   | Tablet' {culoare :: Culoare}
                     deriving (Show)

printColor' :: MobileDevice' -> Culoare
printColor' device = culoare device

result14a = printColor (Smartphone Galben)    -- Output: Galben
result14b = printColor' (Smartphone' Verde)   -- Output: Verde


-- Exercitiul 2.1

data Arb = Frunza Integer | Nod Integer Arb Arb deriving (Show, Eq)


-- Exercitiul 2.2

isBst :: Arb -> Bool
isBst (Frunza _) = True
isBst (Nod valoare stanga dreapta) = if (valoare > (maxVal stanga) && valoare < (minVal dreapta)) then True else False

myArb :: Arb
myArb = Nod 4 (Nod 2 (Frunza 1) (Frunza 3)) (Nod 6 (Frunza 5) (Frunza 7))

result22 = isBst myArb


-- Exercitiul 2.5

maxVal :: Arb -> Integer
maxVal (Frunza valoare) = valoare 
maxVal (Nod valoare stanga dreapta) = max valoare (max (maxVal stanga) (maxVal dreapta))

minVal :: Arb -> Integer
minVal (Frunza valoare) = valoare
minVal (Nod valoare stanga dreapta) = min valoare (min (minVal stanga) (minVal dreapta))

result25a = maxVal myArb
result25b = maxVal myArb
