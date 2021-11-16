{-# LANGUAGE LambdaCase #-}
-- T1 about Functor
module HW2.T1
  ( Annotated (..)
  , Except (..)
  , Fun (..)
  , List (..)
  , Option (..)
  , Pair (..)
  , Prioritised (..)
  , Quad (..)
  , Stream (..)
  , Tree (..)
  , mapAnnotated
  , mapExcept
  , mapFun
  , mapList
  , mapOption
  , mapPair
  , mapPrioritised
  , mapQuad
  , mapStream
  , mapTree
  ) where

data Option a = None | Some a

mapOption :: (a -> b) -> Option a -> Option b
mapOption _ None     = None
mapOption f (Some x) = Some $ f x

data Pair a = P a a

mapPair :: (a -> b) -> Pair a -> Pair b
mapPair f (P x y) = P (f x) (f y)

data Quad a = Q a a a a

mapQuad :: (a -> b) -> Quad a -> Quad b
mapQuad f (Q a b c d) = Q (f a) (f b) (f c) (f d)

data Annotated e a = a :# e deriving Show

infix 0 :#

mapAnnotated :: (a -> b) -> Annotated e a -> Annotated e b
mapAnnotated f (a :# e) = f a :# e

data Except e a = Error e | Success a deriving Show

mapExcept :: (a -> b) -> Except e a -> Except e b
mapExcept _ (Error e)   = Error e
mapExcept f (Success a) = Success $ f a

data Prioritised a = Low a | Medium a | High a

mapPrioritised :: (a -> b) -> Prioritised a -> Prioritised b
mapPrioritised f = \case
 (Low x)    -> Low $ f x
 (Medium x) -> Medium $ f x
 (High x)   -> High $ f x

data Stream a = a :> Stream a

infixr 5 :>

mapStream :: (a -> b) -> Stream a -> Stream b
mapStream f (x :> xs) = f x :> mapStream f xs

data List a = Nil | a :. List a

instance Semigroup (List a) where
  Nil <> l     = l
  a :. as <> l = a :. (as <> l)

mapList :: (a -> b) -> List a -> List b
mapList _ Nil      = Nil
mapList f (a :. l) = f a :. mapList f l

newtype Fun i a = F (i -> a)

mapFun :: (a -> b) -> Fun i a -> Fun i b
mapFun f (F g) = F $ f . g

data Tree a = Leaf | Branch (Tree a) a (Tree a)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Leaf             = Leaf
mapTree f (Branch l val r) = Branch (mapTree f l) (f val) (mapTree f r)

