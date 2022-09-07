{-# LANGUAGE InstanceSigs #-}
import Control.Applicative

--- *** Maybe Applicative ***
-- We'll reimplement the built-in `Maybe` context and make it an Applicative Functor
data Maybe' a = Nothing' | Just' a
  deriving Show

-- Before we can define an Applicative instance, we need to make `Maybe'` a Functor
-- All Applicatives are also Functors: in other words, Functor is a "superclass" of Applicative
instance Functor Maybe' where
   fmap :: (a -> b) -> Maybe' a -> Maybe' b
   fmap _ Nothing'  = Nothing'
   fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
   pure :: a -> Maybe' a
   pure = Just'

-- (+) <$> Just 1 => Just (\x -> 1 + x)

   (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
   Nothing' <*> _ = Nothing'
   _ <*> Nothing' = Nothing'
   Just' f <*> Just' x = Just' (f x)

sumMaybes = (+) <$> Just' 8 <*> Just' 7
--          Just' (\x -> 8 + x) <*> Just' 7 => Just' 15

sumOfThree x y z = x + y + z

sumMaybes' = sumOfThree <$> Just' 8 <*> Just' 7 <*> Just' 5
-- sumMaybes_ = pure sumOfThree <*> Just' 8 <*> Just' 7 <*> Just' 5

-- *** Exercise: Implement the Applicative Instance for List ***
-- Now we will create our own version of the built-in List type from scratch and make it an Applicative:
data List a = Empty | Cons a (List a)

instance Show a => Show (List a) where
  show :: Show a => List a -> String
  show xs = showList True xs
    where
      showList :: Show a => Bool -> List a -> String
      showList True   Empty         = "[]"
      showList True  (Cons x Empty) = "[" ++ show x ++ "]"
      showList True  (Cons x xs)    = "[" ++ show x ++ "," ++ showList False xs
      showList False (Cons x Empty) = show x ++ "]"
      showList False (Cons x xs)    = show x ++ "," ++ showList False xs

-- Built-in List equivalents:
-- Empty == []
-- Cons 1 (Cons 2 (Cons 3 Empty)) == [1, 2, 3]

-- We will need some version of `++` (append) for our List type in our Applicative instance.
-- Recall that we implemented this by making our List a Semigroup, defining `<>` ("mappend"):
instance Semigroup (List a) where
  (<>) :: List a -> List a -> List a
  Empty <> xs = xs
  -- Built-in List equivalent:
  -- [] <> xs = xs
  xs <> Empty = xs
  Cons x xs <> ys = Cons x (xs <> ys)
  -- Built-in List equivalent:
  -- (x:xs) ++ ys = x : xs ++ ys
  --                x : x' : x'' ... : y : y' : y'' ... : []

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Empty = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure :: a -> List a
  pure x = Cons x Empty
  -- Built-in List equivalent:
  -- pure x = [x]

  (<*>) :: List (a -> b) -> List a -> List b
  Empty <*> _ = Empty
  Cons f fs <*> xs = (f <$> xs) <> (fs <*> xs)
  --  Step 1: map the first function to all values in the arguments list
  --  Step 2: append that mapped list to the result of recursively `app`ing the tail of the
  --    functions list to the arguments list
  -- Example: [(+ 1), (* 2), (^ 2)] <*> [1, 2, 3]
  --          (+1) <$> [1, 2, 3] ...
  --                   [2, 3, 4] ++ ((* 2) <$> [1, 2, 3]) ...
  --                                           [2, 4, 6] ++ ((^ 2) <$> [1, 2, 3]) ...
  --                                                                   [1, 4, 9] ++ ([] <*> xs)
  --                                                                                 []
  --                   [2, 3, 4] ++ [2, 4, 6] ++ [1, 4, 9] ++ [] == [2, 3, 4, 2, 4, 6, 1, 4, 9]

combos = Cons (+ 1) (Cons (* 2) (Cons (^ 2) Empty)) <*> Cons 1 (Cons 2 (Cons 3 Empty))
-- i.e. [(+ 1), (* 2), (^ 2)] <*> [1, 2, 3]

-- Exercise Q29.3 (Get Programming with Haskell)
-- You bought soda last night but don't remember whether it was a 6-pack or 12-pack:
startingSoda :: [Int]
startingSoda = [6, 12]

-- You and your roommate each drank 2 sodas yesterday:
remainingSoda :: [Int]
remainingSoda = subtract 4 <$> startingSoda

-- You're having 2 or 3 friends come over:
guests :: [Int]
guests = [2, 3]

-- The total number of people (guests + you + roommate):
totalPeople :: [Int]
totalPeople = (+ 2) <$> guests

-- Each person will drink 3 or 4 sodas:
sodasPerPerson :: [Int]
sodasPerPerson = [3, 4]

-- Calculate how many sodas are needed in total:
sodasNeeded :: [Int]
sodasNeeded = (*) <$> totalPeople <*> sodasPerPerson

-- Calculate how many you need to buy:
sodasToBuy :: [Int]
sodasToBuy = (-) <$> sodasNeeded <*> remainingSoda