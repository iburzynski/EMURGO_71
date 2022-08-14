{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Data.Foldable (foldl')

-- *** Exercise: Scalar & Vector Addition ***
newtype Scalar a = Scalar { getScalar :: a } deriving Show
newtype Vector a = Vector { getVector :: [a] } deriving Show

-- We can create our own `Addable` typeclass using a `class` declaration:
class Addable a where
  -- A class declaration contains a set of method names with accompanying type signatures:
  (+++) :: a -> a -> a
  -- "The `+++` operator takes two values of type `a` and returns a value of type `a`"

-- Now we define `Addable` instances for our Scalar and Vector types:
instance Num a => Addable (Scalar a) where
--       ^ we need a Num constraint on the type inside the Scalars since we need to call (+) on them
--       (+) :: Num a => a -> a -> a
  (+++) (Scalar x) (Scalar y) = Scalar (x + y)

instance Num a => Addable (Vector a) where
  Vector xs +++ Vector ys = Vector $ zipWith (+) xs ys

v1 = Vector [ 1, 2, 3 ]
v2 = Vector [ 4, 5, 6 ]
exVector = v1 +++ v2 -- Vector [5, 7, 9]

newtype Matrix a = Matrix { getMatrix :: [[a]] } deriving Show

instance Num a => Addable (Matrix a) where
  Matrix xss +++ Matrix yss = Matrix $ addMatrix xss yss
    where
      addMatrix [] yss            = yss
      addMatrix xss []            = xss
      addMatrix (xs:xss) (ys:yss) = zipWith (+) xs ys : addMatrix xss yss

m1 = Matrix [ [1, 2, 3]
            , [4, 5, 6]
            , [7, 8, 9]]

m2 = Matrix [ [7, 8, 9]
            , [0, 1, 2] ]

-- *** Exercise: Folding our Custom List ***
data List a = Empty | Cons a (List a)
  deriving (Eq)

myList = Cons 1 (Cons 2 (Cons 3 Empty))
hey = Cons 'h' (Cons 'e' (Cons 'y' Empty))

instance Show a => Show (List a) where
  show :: Show a => List a -> String
  show xs = showList True xs
    where
      showList :: Show a => Bool -> List a -> String
      showList True   Empty         = "[]"
      showList True  (Cons x Empty) = "[" ++ show x ++ "]"
--                                           ^ the `show` applied here is the one defined in the
--                                             `Show` instance for whatever type our List contains
      showList True  (Cons x xs)    = "[" ++ show x ++ ", " ++ showList False xs
      showList False (Cons x Empty) = show x ++ "]"
      showList False (Cons x xs)    = show x ++ ", " ++ showList False xs

-- 1. Define a Foldable instance for our custom List type, so we can fold over it like we can with
--   built-in lists.
instance Foldable List where
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr _ acc Empty = acc
  foldr f acc (Cons x xs) = f x (foldr f acc xs)

-- 2. Then define a version of the `reverse` function for our custom List using a fold.
myReverse :: List a -> List a
myReverse xs = foldl' (flip Cons) Empty xs