module Lib where

type Correct = Bool

checkGuess :: Int -> Int -> (String, Correct)
checkGuess s g = case compare g s of
  EQ -> ("You win!", True)
  LT -> ("Too low - guess again!", False)
  GT -> ("Too high - guess again!", False)