import Test.QuickCheck
import Nat
import NatToInt

instance Arbitrary Nat where
  arbitrary = sized $ \s -> do
    n <- choose (0, s)
    return (intToNat n)

prop_natToInt_intToNat :: Positive Int -> Bool
prop_natToInt_intToNat (Positive x) = natToInt (intToNat x) == x

prop_intToNat_natToInt :: Nat -> Bool
prop_intToNat_natToInt x = intToNat (natToInt x) == x

main :: IO ()
main = do
  quickCheck prop_natToInt_intToNat
  quickCheck prop_intToNat_natToInt