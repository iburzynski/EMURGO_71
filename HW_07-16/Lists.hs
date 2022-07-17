{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

-- Use function patterns to implement the following list functions:

-- Takes any list and returns a string describing its size:
-- "empty", "singleton", "two element list", "three or more element list"
describeList :: [a] -> [Char] -- could also be String
describeList [ ] = "empty"
describeList [_] = "singleton"
describeList [x, y] = "two element list"
describeList (x : xs) = "any other list"

-- implement the built-in `head` function:
myHead :: [a] -> a
myHead [ ] = error "error: empty list"
myHead (x : _) = x

--implement the built-in `tail` function:
myTail :: [a] -> [a]
myTail [ ] = error "error: empty list"
myTail (_ : xs) = xs

-- Homework: implement the following built-in functions to the best of your ability. The name of the
--   built-in function is mentioned above each example between backticks. Use the search feature at
--   http://hoogle.haskell.org to look up the documentation for the corresponding functions and try
--   using them in GHCi to understand how they work before implementing your version.

--- Use function patterns and recursion to define the following functions:

-- `last`:
myLast :: [a] -> a
myLast = undefined

-- `init`:
myInit :: [a] -> [a]
myInit = undefined

-- `length`:
myLength :: [a] -> Int
myLength = undefined

-- `reverse`:
myReverse :: [a] -> [a]
myReverse = undefined

-- `elem`:
myElem :: Eq a => a -> [a] -> Bool
myElem = undefined

-- `(!!)`:
myIndex :: [a] -> Int -> a
myIndex = undefined

-- For the next two functions, use a conditional to make sure the integer argument is > 0
-- What will happen if we don't do this?

-- `take`:
myTake :: Int -> [a] -> [a]
myTake = undefined

-- `drop`:
myDrop :: Int -> [a] -> [a]
myDrop = undefined

-- `sum`:
mySum :: Num a => [a] -> a
mySum = undefined

-- `zip`:
myZip :: [a] -> [b] -> [(a, b)]
myZip = undefined