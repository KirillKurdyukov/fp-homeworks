module HW1.T7
  ( ListPlus (..),
    Inclusive (..),
    DotString (..),
    Fun (..),
  ) where

data ListPlus a = a :+ ListPlus a | Last a

infixr 5 :+

instance Semigroup (ListPlus a) where
  (Last x) <> y  = x :+ y
  (x :+ xs) <> y = x :+ (xs <> y)

data Inclusive a b = This a | That b | Both a b

instance Semigroup (Inclusive a b) where
  (Both x _) <> (Both _ v) = Both x v
  (This x) <> (That y)     = Both x y
  (This x) <> (This _)     = This x
  (That _) <> (That y)     = That y
  (That y) <> (This x)     = This x <> That y
  (This x) <> (Both _ v)   = Both x v
  b@(Both _ _) <> (This _) = b
  (That _) <> b@(Both _ _) = b
  (Both u _) <> (That y)   = Both u y

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
