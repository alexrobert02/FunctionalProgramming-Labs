module Nat (Nat(..), add, sub) where

data Nat = Zero | Succ Nat

instance Show Nat where
  show Zero = "Zero"
  show (Succ n) = "Succ (" ++ show n ++ ")"

instance Eq Nat where
  Zero == Zero = True
  Zero == _    = False
  _    == Zero = False
  (Succ n) == (Succ m) = n == m

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

sub :: Nat -> Nat -> Nat
sub n Zero = n
sub Zero _ = error "Cannot subtract from zero"
sub (Succ m) (Succ n) = sub m n