import System.IO

type Id = String

data Term = Var Id
          | App Term Term
          | Lambda Id Term deriving (Show, Eq)

subst :: Id -> Term -> Term -> Term
subst id term (Var id') | id == id' = term
                        | True = Var id'
subst id term (App term1 term2) = App (subst id term term1) (subst id term term2)
subst id term (Lambda id' term') | id == id' = Lambda id' term'
                                 | True = Lambda id' (subst id term term')

remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (hd:tl) | id == hd = remove id tl
                  | otherwise = hd : remove id tl

vars :: Term -> [Id]
vars (Var id) = [id]
vars (App term1 term2) = vars term1 ++ vars term2
vars (Lambda id term) = id : vars term

free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = free term1 ++ free term2
free (Lambda id term) = remove id (free term)

fresh' :: [Id] -> Int -> Id
fresh' ids index =
  if ("n" ++ show index) `elem` ids
    then fresh' ids (index + 1)
    else "n" ++ show index

fresh :: [Id] -> Id
fresh ids = fresh' ids 1

casubst :: Id -> Term -> Term -> [Id] -> Term
casubst id term (Var id') _ | id == id' = term
                            | otherwise = Var id'
casubst id term (App term1 term2) avoid = App (casubst id term term1 avoid) (casubst id term term2 avoid)
casubst id term (Lambda id' term') avoid
  | id == id' = Lambda id' term'
  | id' `elem` free term = let freshId = fresh (avoid ++ vars term ++ free term) in
                            Lambda freshId (casubst id term (subst id' (Var freshId) term') (avoid ++ [freshId]))
  | otherwise = Lambda id' (casubst id term term' avoid)

reduce1' :: Term -> [Id] -> Maybe Term
reduce1' (Var id') _ = Nothing
reduce1' (App (Lambda id term) term') avoid =
  Just (casubst id term' term avoid)
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

reduce :: Term -> IO Term
reduce term = do
  case reduce1 term of
    Nothing -> return term
    Just term' -> reduce term'


-- Exercitiul 0.1

--  Strategia de evaluare implementata mai sus este call-by-value. In aceasta strategie, argumentele functiei sunt evaluate in intregime
--iainte ca functia sa fie aplicata.

--  In primul exemplu (λx1.x1) ((λx2.x2) (λz.(λx3.x3) z)), se evalueaza intai argumentul (λx2.x2) la el insusi deoarece este o valoare.
--  Apoi evaluam (λz.(λx3.x3) z) substituind z la (λx3.x3).
--  In final, substituim rezultatul la (λx1.x1).

-- (λx1.x1) ((λx2.x2) (λz.(λx3.x3) z))   -- Se aplica cel mai departe redex
-- (λx1.x1) ((λx2.x2) (λz.z))            -- Se reduce (λz.(λx3.x3) z)
-- (λx1.x1) (λz.z)                       -- Se reduce (λx2.x2)
-- λz.z                                  -- Se reduce (λx1.x1)

-- Pentru urmatorul exemplu (λx1.λx2.x2)((λx.x) (λy.y)), evaluam mai întai argumentul ((λx.x) (λy.y)). 
-- Substituim (λy.y) in (λx.x) și obținem (λy.y). Apoi înlocuim termenul rezultat în (λx1.λx2.x2). 
  
-- (λx1.λx2.x2)((λx.x) (λy.y))   -- Se aplica cel mai departe redex
--(λx1.λx2.x2)(λy.y)             -- Se reduce (λx.x)
-- λx2.x2                        -- Se Reduce (λx1.λx2.x2)


-- Exercitiul 0.2

reduce2' :: Term -> [Id] -> Maybe Term
reduce2' (Var id') _ = Nothing
reduce2' (App (Lambda id term) term') avoid =
  Just (casubst id term' term avoid)
reduce2' (App term1 term2) avoid =
  case reduce2' term1 avoid of
    Nothing -> Just (App term1 term2)
    Just term1' -> Just (App term1' term2)
reduce2' (Lambda _ _) _ = Nothing

reduce2 :: Term -> Maybe Term
reduce2 t = reduce2' t (vars t)

reduce02 :: Term -> IO Term
reduce02 term = do
  case reduce2 term of
    Nothing -> return term
    Just term' -> reduce02 term'

-- (λx1.x1 x1) ((λx.x) (λy.y))
-- Folosind strategia CBN, evaluam aplicatia functiei cele mai exterioare. 
-- Inlocuim argumentul ((λx.x) (λy.y)) fara a-l reduce în continuare.

-- (λx1.x1 x1) ((λx.x) (λy.y))     -- Apply the outermost redex
-- ((λx.x) (λy.y)) ((λx.x) (λy.y)) -- Substitute the argument without reduction
-- (λy.y) ((λx.x) (λy.y))          -- Reduce (λx.x) to itself
-- (λy.y) (λy.y)                   -- Reduce (λx.x) (λy.y) to (λy.y)
-- y                               -- Reduce (λy.y) (λy.y)

example02 = App (Lambda "x1" (App (Var "x1") (Var "x1"))) (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y")))


-- Exercitiul 0.3

--  Funcțiile strategie1’, strategie1 și strategie implementează strategia normal order. 
--  Este o strategie de evaluare care colectează toate reducerile posibile într-un singur pas la fiecare pas, explorând diferite căi de reducere.

strategy1' :: Term -> [Id] -> [Term]
strategy1' (Var _) _ = []
strategy1' (App (Lambda id term) term') avoid = [casubst id term' term avoid] ++
  let all = strategy1' term avoid in
  let all' = strategy1' term' avoid in
  [ App (Lambda id successorTerm) term' | successorTerm <- all ] ++
  [ App (Lambda id term) successorTerm' | successorTerm' <- all']
strategy1' (App term1 term2) avoid =
  let all1 = strategy1' term1 avoid in
  let all2 = strategy1' term2 avoid in
  [ App term1' term2 | term1' <- all1 ] ++
  [ App term1 term2' | term2' <- all2 ]
strategy1' (Lambda id term) avoid =
  let all = strategy1' term avoid in
  [ Lambda id sterm | sterm <- all ]

strategy1 :: Term -> [Term]
strategy1 term = strategy1' term (vars term)

strategy :: Term -> [Term]
strategy term = let all = strategy1 term in case all of
    [] -> [term]
    _ -> concat (map strategy all)

--  (λx1.x1) ((λx2.x2) (λz.(λx3.x3) z))
example03 = App (Lambda "x1" (Var "x1")) (App (Lambda "x2" (Var "x2")) (App (Lambda "z" (Lambda "x3" (Var "x3"))) (Var "z")))
result03 = strategy example03

-- Rezultatul va fi o lista de termeni reprezentand toate secventele posibile de reducere. 
-- Deoarece pot exista mai multe cai de reducere, rezultatul real poate varia, dar va include urmatorul termen: λ3.x3


-- Exercitiul 0.4

cbvReduce1' :: Term -> [Id] -> Maybe Term
cbvReduce1' (Var id') _ = Nothing
cbvReduce1' (App (Lambda id term) term') avoid =
  case cbvReduce1' term avoid of
    Nothing -> Just (casubst id term' term avoid)
    Just term'' -> Just (App (Lambda id term'') term')
cbvReduce1' (App term1 term2) avoid =
  case cbvReduce1' term1 avoid of
    Nothing ->
      case cbvReduce1' term2 avoid of
        Nothing -> Nothing
        Just term2' -> Just (App term1 term2')
    Just term1' -> Just (App term1' term2)
cbvReduce1' (Lambda _ _) _ = Nothing

cbvReduce1 :: Term -> Maybe Term
cbvReduce1 t = cbvReduce1' t (vars t)
  where
    vars (Var id) = [id]
    vars (App t1 t2) = vars t1 ++ vars t2
    vars (Lambda id term) = id : vars term

cbvStrategy :: Term -> [Term]
cbvStrategy term =
  case cbvReduce1 term of
    Nothing -> [term]
    Just term' -> term : cbvStrategy term'

result04 = cbvStrategy example03
