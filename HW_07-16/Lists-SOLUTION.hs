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
myLast [x]    = x
myLast (_:xs) = myLast xs
myLast []     = error "empty list"

-- `init`:
myInit :: [a] -> [a]
myInit [_]    = []
myInit (x:xs) = x : myInit xs
-- myInit [1, 2, 3] => 1 : myInit [2, 3]
--                         2 : myInit [3]
--                             []
--                     1 : 2 : [] => [1, 2]
myInit []     = error "empty list"

-- `length`:
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs
  --     (1: [2, 3]) = 1 + myLength [2, 3]
  --                         1 + myLength [3]
  --                               1 + myLength []
  --                                     0

-- `reverse`:
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


-- `elem`:
myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem q (x:xs) = x == q || myElem q xs
-- myElem q (x:xs)
--  | q == x = True
--  | otherwise = myElem q xs

-- `(!!)`:
myIndex :: [b] -> Int -> b
myIndex _ i
  | i < 0 = error "negative index"
myIndex [] _ = error "index too large"
myIndex (x:_)  0 = x
myIndex (_:xs) i = myIndex xs (i - 1)

myIndex' (x:xs) i
  | i < 0 = error "negative index"
  | i == 0 = x
  | otherwise = myIndex' xs (i - 1)
myIndex' [] _ = error "index too large"

-- myIndex (x:xs) i = myIndex xs . (`subtract` 1) $ i
-- myIndex (x:xs) i = myIndex xs $ i - 1

-- For the next two functions, use a conditional to make sure the integer argument is > 0
-- What will happen if we don't do this?

-- `take`:
myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake n (x:xs)
  | n < 1 = []
  | otherwise = x : myTake (n - 1) xs

-- `drop`:
myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop n l@(_:xs) -- alias operator `@` allows us to assign a name to the entire value to the right
  | n < 1 = l -- use the alias `l` we created to reference the entire list
  | otherwise = myDrop (n - 1) xs

-- Alternate syntax (without aliasing):
-- myDrop n (x:xs)
--   | n < 1 = x : xs
--   | otherwise = myDrop (n - 1) xs

-- `sum`:
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

-- `zip`:
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys
--                    (x0, y0) : (x1, y1) : (x2, y2) ... : [] => [(x0, y0), (x1, y1), (x2, y2)...]