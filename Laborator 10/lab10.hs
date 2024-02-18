import Prelude hiding (Right, Left)

data Tree = Nil | Node Int Tree Tree deriving (Show, Eq)

t :: Tree
t1 = Node 11 Nil Nil
t2 = Node 44 Nil Nil
t3 = Node 15 Nil Nil
t4 = Node 6 Nil Nil
t = Node 2 (Node 56 t1 t2) (Node 8 t3 t4)

data Dir = L | R deriving (Show, Eq)

type Pos = [Dir]

pos :: Pos
pos = [R, L]

atPos :: Tree -> Pos -> Int
atPos (Node x l r) [] = x
atPos (Node x l r) (L:tl) = atPos l tl
atPos (Node x l r) (R:tl) = atPos r tl


-- Exercitiul 1
-- Arbori binari: Modificati functiile change, goLeft, goRight si goUp pentru a intoarce Maybe Zipper.

change :: Tree -> Pos -> Int -> Tree
change (Node x l r) [] v = Node v l r
change (Node x l r) (L:tl) v = Node x (change l tl v) r
change (Node x l r) (R:tl) v = Node x l (change r tl v)

goLeft1 :: (Tree, [Dir]) -> (Tree, [Dir])
goLeft1 (Node x l r, dirs) = (l, L : dirs)

goRight1 :: (Tree, [Dir]) -> (Tree, [Dir])
goRight1 (Node x l r, dirs) = (r, R : dirs)

change1 :: (Tree, [Dir]) -> Int -> (Tree, [Dir])
change1 (Node x l r, dirs) v = (Node v l r, dirs)

data Crumb = Left Int Tree | Right Int Tree deriving (Show, Eq)
type Zipper =(Tree, [Crumb])


-- Exercitiul 2
-- Folositi >>= in cateva exemple de inlantuiri de apeluri de goLeft/goUp/goRight/change.

goLeft2 :: Zipper -> Maybe Zipper
goLeft2 (Node x l r, crumbs) = Just (l, Left x r : crumbs)
goLeft2 (Nil,crumbs)=Just(Nil, crumbs);

goRight2 ::Zipper -> Maybe Zipper
goRight2 (Node x l r, crumbs) =Just (r, Right x l : crumbs)
goRight2 (Nil,crumbs)=Just(Nil, crumbs);

goUp2 :: Zipper -> Maybe Zipper
goUp2 (t, Left x r : crumbs) = Just (Node x t r, crumbs)
goUp2 (t, Right x l : crumbs) = Just (Node x l t, crumbs)

change2 :: Int -> Zipper -> Zipper
change2 v (Node x l r, crumbs) = (Node v l r, crumbs)

type ListZipper a = ([a],[a])


-- Exercitiul 3
-- Liste: Modificati functiile change, goFwd, goBwd pentru a intoarce Maybe Zipper.

goFwd :: ListZipper a -> ListZipper a  
goFwd (x:xs, bs) = (xs, x:bs)  

goBack :: ListZipper a -> ListZipper a  
goBack (xs, b:bs) = (b:xs, bs)  

main :: IO ()
main = case goRight2 (Node 2 (Node 3 Nil Nil) (Node 4 Nil Nil), []) >>= goLeft2 of
  Just (t, _) -> print t
  Nothing -> putStrLn "Failed to traverse the tree."
