{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use or" #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Use product" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use and" #-}

import Data.List (foldl')
----------------------------------------------------------------------------------------------------
--                          *** HOMEWORK: Higher-Order Functions ***
----------------------------------------------------------------------------------------------------

-- *** Part I: Map/Filter ***
-- Implement your own versions of the `map` and `filter` functions using primitive recursion.

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x : xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x : xs)
  | p x = x : myFilter p xs
  | otherwise = myFilter p xs

-- *** Part II: Folds ***
-- Implement the following built-in functions using folds.
-- For each solution, think carefully about the following:
--  1. Which type of fold should be used?
--      * Use `foldr` whenever possible
--      * If a left fold is needed, use the strict version `foldl'` (imported from Data.List module)
--  2. Which binary function should be used for the reducer?
--      * Is there a built-in operator or function you can use?
--      * If not, write your own using a lambda expression
--      * Be mindful of parameter order!
--  3. What is the initial (identity) value?

mySum :: Num a => [a] -> a
mySum xs = foldr (+) 0 xs
-- Point-free version:
-- mySum = foldr (+) 0

myProduct :: Num a => [a] -> a
myProduct xs = foldr (*) 1 xs

-- For the following boolean functions, remember the built-in logical operators (||) and (&&):
-- and [True, False, True]   == False
-- and [True, True, True]    == True
-- or  [True, False, True]   == True
-- or  [False, False, False] == False

-- Fold Arguments
-- 1. Binary function (reducer)
-- 2. Initial (identity) value
-- 3. Collection (foldable)

myAnd :: [Bool] -> Bool
-- https://www.stackage.org/haddock/lts-19.6/base-4.15.1.0/Prelude.html#v:and
myAnd bs = foldr (&&) True bs

myOr :: [Bool] -> Bool
-- https://www.stackage.org/haddock/lts-19.6/base-4.15.1.0/Prelude.html#v:or
myOr = foldr (||) False

myElem :: Eq a => a -> [a] -> Bool
myElem q xs = foldr (\x acc -> acc || q == x) False xs

-- The next 3 functions take a predicate (function returning a Bool) as their first parameter

myAny :: (a -> Bool) -> [a] -> Bool
-- https://www.stackage.org/haddock/lts-19.6/base-4.15.1.0/Prelude.html#v:any
myAny p xs = foldr (\x acc -> acc || p x) False xs

myAll :: (a -> Bool) -> [a] -> Bool
-- https://www.stackage.org/haddock/lts-19.6/base-4.15.1.0/Prelude.html#v:all
myAll p xs = foldr (\x acc -> acc && p x) True xs

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' p xs = foldr (\x acc ->
  if p x
  then x : acc
  else acc) [] xs

myMap' :: (a -> b) -> [a] -> [b]
myMap' f xs = foldr (\x acc -> f x : acc) [] xs

-- Implement `reverse` using `foldr`.
myReverseR :: [a] -> [a]
myReverseR xs = foldr (\x acc -> acc ++ [x]) [] xs

-- Implement `reverse` using `foldl'`.
-- For a challenge, you can try using the `flip` function: flip :: (a -> b -> c) -> (b -> a -> c)
-- https://www.stackage.org/haddock/lts-19.6/base-4.15.1.0/Prelude.html#v:flip
myReverseL :: [a] -> [a]
myReverseL xs = foldl' (\acc x -> x : acc) [] xs
myReverseL' xs = foldl' (flip (:)) [] xs

-- The next two functions are partial functions: they return an error when called on an empty list.
-- Implement the non-empty patterns using folds:
-- Hint: for the initial/identity value, use the head of the list and fold over the tail.

myMaximum :: Ord a => [a] -> a
--https://www.stackage.org/haddock/lts-19.6/base-4.15.1.0/Prelude.html#v:maximum
myMaximum [] = error "empty list"
myMaximum (x:xs) = foldr max x xs

myMinimum :: Ord a => [a] -> a
--https://www.stackage.org/haddock/lts-19.6/base-4.15.1.0/Prelude.html#v:minimum
myMinimum [] = error "empty list"
myMinimum (x:xs) = foldr min x xs