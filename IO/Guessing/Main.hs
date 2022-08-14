{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use <$>" #-}
module Main where
import Lib

-- Using bind/sequence:
main' :: IO ()
main' = putStrLn "Enter the secret number:" >> getNumber >>= (\secret -> processGuess secret)

-- Using do-notation:
main :: IO ()
main = do
  putStrLn "Enter the secret number:"
  secret <- getNumber
  processGuess secret

-- Using bind/sequence:
processGuess' :: Int -> IO ()
processGuess' secret = putStrLn "Enter your guess:" >> getNumber >>= go
  where
   go g = case checkGuess secret g of
     (s, True)  -> putStrLn s -- if guess is correct, print the winning message and terminate
     (s, False) -> putStrLn s >> processGuess' secret -- else print the message and loop

-- Using do-notation:
processGuess :: Int -> IO ()
processGuess secret = do
  putStrLn "Enter your guess:"
  guess <- getNumber
  case checkGuess secret guess of
    (s, True)  -> putStrLn s
    (s, False) -> do
      putStrLn s
      processGuess secret

-- Using bind/return:
getNumber' :: IO Int
getNumber' = getLine >>= convertToInt
 where
--  convertToInt :: String -> IO Int
 convertToInt = (\s -> return (read s))

-- Using do-notation/return:
getNumber :: IO Int
getNumber = do
  n <- getLine
  return $ read n