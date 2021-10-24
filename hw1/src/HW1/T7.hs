module HW1.T7
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a

infixr 5 :+

instance Semigroup (ListPlus a) where
  (Last x) <> y  = x :+ y
  (x :+ xs) <> y = x :+ (xs <> y)

data Inclusive a b = This a | That b | Both a b

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (Both x y) <> (Both u v) = Both (x <> u) $ y <> v
  (This x) <> (That y)     = Both x y
  (This x) <> (This y)     = This $ x <> y
  (That x) <> (That y)     = That $ x <> y
  (That y) <> (This x)     = This x <> That y
  (This x) <> (Both u v)   = Both (x <> u) v
  (Both x y) <> (This u)   = Both (x <> u) y
  (That x) <> (Both u v)   = Both u $ x <> v
  (Both x y) <> (That u)   = Both x $ y <> u

newtype DotString = DS String
  deriving (Show)

instance Semigroup DotString where
  (DS x) <> (DS "") = DS x
  (DS "") <> (DS x) = DS x
  (DS x) <> (DS y)  = DS $ x ++ ['.'] ++ y

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (F x) <> (F y) = F $ x . y

instance Monoid (Fun a) where
  mempty = F id
