type Id = String

data Term = Var Id
        | App Term Term
        | Lambda Id Term deriving (Show, Eq)


-- Exercitiul 0.1

lambdaterm :: Term
lambdaterm = Lambda "x" (Lambda "y" (Var "x"))


-- Exercitiul 0.2

subst :: Id -> Term -> Term -> Term
subst id term (Var id') | id == id' = term
                        | True = Var id'
subst id term (App term1 term2) = App (subst id term term1) (subst id term term2)
subst id term (Lambda id' term') | id == id' = Lambda id' term'
                                 | True = Lambda id' (subst id term term')


-- Exercitiul 0.3

remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (hd:tl) | id == hd = remove id tl
                  | otherwise = hd : remove id tl


-- Exercitiul 0.4

free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = free term1 ++ free term2
free (Lambda id term) = remove id (free term)


-- Exercitiul 0.5

vars :: Term -> [Id]
vars (Var id) = [id]
vars (App term1 term2) = vars term1 ++ vars term2
vars (Lambda id term) = id : vars term


-- Exercitiul 0.6

fresh' :: [Id] -> Int -> Id
fresh' ids index =
  if ("n" ++ show index) `elem` ids
    then fresh' ids (index + 1)
    else "n" ++ show index

fresh :: [Id] -> Id
fresh ids = fresh' ids 1


-- Exercitiul 0.7

casubst :: Id -> Term -> Term -> [Id] -> Term
casubst id term (Var id') _ | id == id' = term
                            | otherwise = Var id'
casubst id term (App term1 term2) avoid = App (casubst id term term1 avoid) (casubst id term term2 avoid)
casubst id term (Lambda id' term') avoid
  | id == id' = Lambda id' term'
  | id' `elem` free term = let freshId = fresh (avoid ++ vars term ++ free term) in
                            Lambda freshId (casubst id term (subst id' (Var freshId) term') (avoid ++ [freshId]))
  | otherwise = Lambda id' (casubst id term term' avoid)


-- Exercitiul 0.8

reduce1' :: Term -> [Id] -> Maybe Term
reduce1' (Var _) _ = Nothing
reduce1' (App (Lambda id term) term') avoid =
  Just (casubst id term' term avoid) -- the actual beta-reduction
reduce1' (App term1 term2) avoid =
  case reduce1' term1 avoid of
    Nothing ->
      case reduce1' term2 avoid of
        Nothing -> Nothing
        Just term2' -> Just (App term1 term2')
    Just term1' -> Just (App term1' term2)
reduce1' (Lambda id term) avoid =
  case reduce1' term avoid of
    Nothing -> Nothing
    Just term' -> Just (Lambda id term')

reduce1 :: Term -> Maybe Term
reduce1 t = reduce1' t (vars t)


-- Exercitiul 0.9

reduce :: Term -> Term
reduce term =
  case reduce1 term of
    Nothing -> term
    Just term' -> reduce term'


-- Exercitiul 0.10

reduceFor :: Int -> Term -> Term
reduceFor 0 term = term
reduceFor n term =
  case reduce1 term of
    Nothing -> term
    Just term' -> reduceFor (n - 1) term'

loopTerm = App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "x" (App (Var "x") (Var "x")))
result10 = reduceFor 10 loopTerm


-- Exercitiul 0.11

tTRUE = Lambda "x" (Lambda "y" (Var "x"))
tFALSE = Lambda "x" (Lambda "y" (Var "y"))
tAND = Lambda "p" (Lambda "q" (App (App (Var "p") (Var "q")) (Var "p")))
tOR = Lambda "p" (Lambda "q" (App (App (Var "p") (Var "p")) (Var "q")))
tNOT = Lambda "p" (App (App (Var "p") tFALSE) tTRUE)

term11 = App (App tAND tTRUE) tFALSE
result11 = reduce term11

term1 = App (App tAND tTRUE) tTRUE
normalForm1 = reduce term1

term2 = App (App tOR tFALSE) tTRUE
normalForm2 = reduce term2

term3 = App tNOT tTRUE
normalForm3 = reduce term3


-- Exercitiul 0.12

tPLUS = Lambda "m" (Lambda "n" (Lambda "s" (Lambda "z" (App (App (Var "m") (Var "s")) (App (App (Var "n") (Var "s")) (Var "z"))))))
tMULT = Lambda "m" (Lambda "n" (Lambda "s" (Lambda "z" (App (App (Var "m") (App (Var "n") (Var "s"))) (Var "z")))))

two = Lambda "s" (Lambda "z" (App (Var "s") (App (Var "s") (Var "z"))))
three = Lambda "s" (Lambda "z" (App (Var "s") (App (Var "s") (App (Var "s") (Var "z")))))

plusResult = reduce (App (App tPLUS two) three)
multResult = reduce (App (App tMULT two) three)


-- Exercitiul 0.13

alphaEq :: Term -> Term -> Bool
alphaEq (Var x) (Var y) = x == y
alphaEq (Lambda x t1) (Lambda y t2) = alphaEq t1 (casubst y (Var x) t2 [x])
alphaEq (App t1 t2) (App t3 t4) = alphaEq t1 t3 && alphaEq t2 t4
alphaEq _ _ = False

term131 = App (App (Lambda "x" (Lambda "y" (Var "x"))) (Var "a")) (Var "b")
term132 = App (App (Lambda "y" (Lambda "z" (Var "y"))) (Var "a")) (Var "b")

result13 = alphaEq term131 term132 --True
