module NatToInt where

import Nat

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

intToNat :: Int -> Nat
intToNat n
  | n < 0 = error "Cannot convert negative numbers"
  | otherwise = intToNat' n
  where
    intToNat' 0 = Zero
    intToNat' n = Succ (intToNat' (n - 1))