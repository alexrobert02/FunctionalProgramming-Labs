module Main where

import Nat
import NatToInt

main :: IO ()
main = do
    let n1 = 3
    let n2 = 4
    putStrLn $ "n1 = " ++ show n1
    putStrLn $ "n2 = " ++ show n2
    let n3 = intToNat n1
    let n4 = intToNat n2
    putStrLn $ "Converted n1 = " ++ show n3
    putStrLn $ "Converted n2 = " ++ show n4   
    let n5 = add n3 n4
    putStrLn $ "Converted n1 + n2 = " ++ show n5
    let n6 = natToInt n5
    putStrLn $ "n1 + n2 = " ++ show n6
    