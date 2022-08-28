{-# LANGUAGE InstanceSigs #-}
import qualified Data.Map as M
import Data.Map (Map)

data List a = Empty | Cons a (List a)

instance Show a => Show (List a) where
  show :: Show a => List a -> String
  show xs = showList True xs
    where
      showList :: Show a => Bool -> List a -> String
      showList True   Empty         = "[]"
      showList True  (Cons x Empty) = "[" ++ show x ++ "]"
      showList True  (Cons x xs)    = "[" ++ show x ++ ", " ++ showList False xs
      showList False (Cons x Empty) = show x ++ "]"
      showList False (Cons x xs)    = show x ++ ", " ++ showList False xs

listMap :: (a -> b) -> (List a -> List b)
listMap _ Empty = Empty
listMap f (Cons x xs) = Cons (f x) (listMap f xs)

mapMaybe :: (a -> b) -> (Maybe a -> Maybe b)
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

instance Functor List where
  fmap :: (a -> b) -> (List a -> List b)
  fmap _ Empty = Empty
  fmap f (Cons x xs) = Cons (f x) (listMap f xs)

-- instance Functor Maybe where
--   fmap :: (a -> b) -> (Maybe a -> Maybe b)
--   fmap _ Nothing = Nothing
--   fmap f (Just x) = Just (f x)

ioInt :: IO Int
ioInt = return 7

-- The Functor typeclass provides a common interface to apply any function to a value in a context.

data Shape a b = Rectangle a b | Circle b
  deriving Show

instance Functor (Shape a) where
  fmap :: (b -> c) -> (Shape a b -> Shape a c)
  fmap f (Rectangle h w) = Rectangle h (f w)
  fmap f (Circle r) = Circle (f r)

-- f <$> (Circle r) = Circle (f r)

data RobotPart = RobotPart
  { name :: String
  , description :: String
  , cost :: Double
  , count :: Int
  } deriving Show

leftArm :: RobotPart
leftArm = RobotPart
  { name = "left arm"
  , description = "left arm for face punching!"
  , cost = 1000.00
  , count = 3
  }

rightArm :: RobotPart
rightArm = RobotPart
  { name = "right arm"
  , description = "right arm for kind hand gestures"
  , cost = 1025.00
  , count = 5
  }

robotHead :: RobotPart
robotHead = RobotPart
  { name = "robot head"
  , description = "this head looks mad"
  , cost = 5092.25
  , count = 2
  }

-- Rendering a Robot Part as Html

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = concat [ "<h2>"
                         , partName
                         , "</h2>"
                         , "<p><h3>desc</h3>"
                         , partDesc
                         , "</p><p><h3>cost</h3>"
                         , partCost
                         , "</p><p><h3>count</h3>"
                         , partCount
                         , "</p>"]
  where
    partName = name part
    partDesc = description part
    partCost = show (cost part)
    partCount = show (count part)


partsDB :: Map Int RobotPart
partsDB = M.fromList $ zip [1, 2, 3] [leftArm, rightArm, robotHead]

-- Converting a Maybe RobotPart to Maybe Html

partVal :: Maybe RobotPart
partVal = M.lookup 1 partsDB

renderMaybeHtml :: Maybe RobotPart -> Maybe Html
renderMaybeHtml Nothing = Nothing
renderMaybeHtml (Just part) = Just (renderHtml part)


partHtml :: Maybe Html
partHtml = renderHtml <$> partVal
-- same as: fmap renderHtml partVal

-- Converting a list of Robot Parts to a list of HTML

allParts :: [RobotPart]
allParts = map snd (M.toList partsDB)

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts
-- same as: map renderHtml allParts

-- Converting a Map of Robot Parts to HTML

htmlPartsDB :: M.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

ex1 = M.lookup 1 htmlPartsDB

--Transforming an IO Robot Part to IO Html

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

