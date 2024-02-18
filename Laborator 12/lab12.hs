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

reduce :: Term -> Term
reduce term =
  case reduce1 term of
    Nothing -> term
    Just term' -> reduce term'


-- Exercitiul 0.1

myTrue :: Term
myTrue = Lambda "x" (Lambda "y" (Var "x"))

resultMyTrue = reduce (App myTrue myTrue)

identity :: Term
identity = Lambda "x" (Var "x")

resultIdentity1 :: Maybe Term
resultIdentity1 = reduce1 (App identity (Lambda "y" (Var "y")))

resultIdentity2 :: Term
resultIdentity2 = reduce (App identity (Lambda "y" (Var "y")))


-- Exercitiul 0.2

-- 1.
true :: Term
true = Lambda "t" (Lambda "f" (Var "t"))

false :: Term
false = Lambda "t" (Lambda "f" (Var "f"))

andOp :: Term
andOp = Lambda "p" (Lambda "q" (App (App (Var "p") (Var "q")) false))

orOp :: Term
orOp = Lambda "p" (Lambda "q" (App (App (Var "p") true) (Var "q")))

notOp :: Term
notOp = Lambda "p" (App (App (Var "p") false) true)

testAnd = reduce (App (App andOp true) false)
testOr = reduce (App (App orOp true) false)
testNot = reduce (App notOp true)

-- 2.
zero :: Term
zero = Lambda "s" (Lambda "z" (Var "z"))

succOp :: Term
succOp = Lambda "n" (Lambda "s" (Lambda "z" (App (Var "s") (App (App (Var "n") (Var "s")) (Var "z")))))

plusOp :: Term
plusOp = Lambda "m" (Lambda "n" (Lambda "s" (Lambda "z" (App (App (Var "m") (Var "s")) (App (App (Var "n") (Var "s")) (Var "z"))))))

multOp :: Term
multOp = Lambda "m" (Lambda "n" (Lambda "s" (Lambda "z" (App (App (Var "m") (App (Var "n") (Var "s"))) (Var "z")))))

testSucc = reduce (App succOp zero)
testPlus = reduce (App (App plusOp (reduce (App succOp (reduce (App succOp zero))))) (reduce (App succOp zero)))
testMult = reduce (App (App multOp (reduce (App succOp (reduce (App succOp zero))))) (reduce (App succOp zero)))

-- 3.
isZeroOp :: Term
isZeroOp = Lambda "n" (App (App (Var "n") (Lambda "_" false)) true)

leqOp :: Term
leqOp = Lambda "m" (Lambda "n" (App (App isZeroOp (App (App plusOp (Var "n")) (Var "m"))) false))

eqOp :: Term
eqOp = Lambda "m" (Lambda "n" (App (App andOp (App (App leqOp (Var "m")) (Var "n"))) (App (App leqOp (Var "n")) (Var "m"))))

testIsZero = reduce (App isZeroOp zero)
testLeq = reduce (App (App leqOp (reduce (App succOp zero))) (reduce (App succOp zero)))
testEq = reduce (App (App eqOp (reduce (App succOp zero))) (reduce (App succOp zero)))

-- 4.
pairOp :: Term
pairOp = Lambda "x" (Lambda "y" (Lambda "f" (App (App (Var "f") (Var "x")) (Var "y"))))

fstOp :: Term
fstOp = Lambda "p" (App (Var "p") true)

sndOp :: Term
sndOp = Lambda "p" (App (Var "p") false)

testPair = reduce (App (App pairOp (reduce (App succOp zero))) (reduce (App succOp (reduce (App succOp zero)))))
testFst = reduce (App fstOp (reduce (App (App pairOp (reduce (App succOp zero))) (reduce (App succOp (reduce (App succOp zero)))))))
testSnd = reduce (App sndOp (reduce (App (App pairOp (reduce (App succOp zero))) (reduce (App succOp (reduce (App succOp zero)))))))

-- 5.
nilOp :: Term
nilOp = false

consOp :: Term
consOp = Lambda "h" (Lambda "t" (Lambda "c" (App (App (Var "c") (Var "h")) (Var "t"))))

headOp :: Term
headOp = fstOp

tailOp :: Term
tailOp = sndOp

isNilOp :: Term
isNilOp = notOp

testCons = reduce (App (App consOp (reduce (App succOp zero))) (reduce (App (App consOp (reduce (App succOp (reduce (App succOp zero))))) nilOp)))
testHead = reduce (App headOp (reduce (App (App consOp (reduce (App succOp zero))) (reduce (App (App consOp (reduce (App succOp (reduce (App succOp zero))))) nilOp)))))
testTail = reduce (App tailOp (reduce (App (App consOp (reduce (App succOp zero))) (reduce (App (App consOp (reduce (App succOp (reduce (App succOp zero))))) nilOp)))))
testIsNil = reduce (App isNilOp nilOp)
