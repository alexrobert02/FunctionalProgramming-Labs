import System.Environment
import System.IO
import Control.Exception
import Data.Char (toUpper)


-- Exercitiul 0.5

main5 :: IO ()
main5 = putStrLn "What is your name?" >> 
        getLine >>= \name ->
        putStrLn ("Hello, " ++ name ++ "!")


-- Exercitiul 0.6 

main6 :: IO ()
main6 = putStrLn "What is your name?" >> 
        getLine >>= \name ->
        putStrLn ("Hello, " ++ name ++ "!") >>
        main6


-- Exercitiul 0.7

main7 :: IO ()
main7 = do putStrLn "What is your name?"
           name <- getLine
           if name == "" then
             return ()
            else
             do putStrLn ("Hello, " ++ name ++ "!")
                main7


-- Exercitiul 0.8

main8 :: IO ()
main8 = do
        putStrLn "Scrieti un string:"
        str <- getLine
        putStrLn $ map toUpper str


-- Exercitiul 0.10

main10 :: IO ()
main10 = do handle <- openFile "example.txt" ReadMode
            contents <- hGetContents handle
            putStrLn contents


-- Exercitiul 0.11

main11 :: IO ()
main11 = do args <- getArgs
            let fileName = head args
            handle <- openFile fileName ReadMode
            contents <- hGetContents handle
            putStrLn contents


-- Exercitiul 0.12

main12 :: IO ()
main12 = do args <- getArgs
            case args of
              [] -> do progName <- getProgName
                       putStrLn $ "./" ++ progName ++ " <filename>"
              (hd:_) -> do handle <- openFile hd ReadMode
                           contents <- hGetContents handle
                           putStrLn contents


-- Exercitiul 0.13

main13 :: IO ()
main13 = do args <- getArgs
            case args of
              [] -> do progName <- getProgName
                       putStrLn $ "./" ++ progName ++ " <filename>"
              (hd:_) -> do handle <- openFile hd ReadMode
                           contents <- hGetContents handle
                           putStrLn $ proceseaza contents

proceseaza :: String -> String
proceseaza = map toUpper


-- Exercitiul 0.14

main14 :: IO ()
main14 = guessNumber 0 100

guessNumber :: Int -> Int -> IO ()
guessNumber lower higher
  | lower == higher = putStrLn $ "Atunci numarul este " ++ show lower ++ "."
  | otherwise = do let mid = if (lower + higher) `mod` 2 == 0 
                               then (lower + higher) `div` 2 
                               else 1 + (lower + higher) `div` 2
                   putStrLn $ "Numarul de ghicit este >= " ++ show mid ++ "? "
                   raspuns <- getLine
                   case raspuns of
                      "Da" -> guessNumber mid higher
                      "Nu" -> guessNumber lower (mid - 1)   
                      _ -> do putStrLn "Nu am inteles. Raspunde cu 'Da' sau 'Nu'."
                              guessNumber lower higher

main :: IO ()
main = main14