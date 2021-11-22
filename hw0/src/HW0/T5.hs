{-# LANGUAGE LambdaCase #-}

module HW0.T5
  ( Nat 
  , nFromNatural
  , nToNum
  , nmult
  , nplus
  , ns
  , nz
  ) where

import           GHC.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz = id

--((a -> a) -> a -> a) -> ((a -> a) -> a -> a)
ns :: Nat a -> Nat a
ns f g = g . f g

--((a -> a) -> a -> a) -> ((a -> a) -> a -> a) -> (a -> a) -> a -> a
nplus, nmult :: Nat a -> Nat a -> Nat a
nplus a b f = a f . b f
nmult a b = a . b

nFromNatural :: Natural -> Nat a
nFromNatural = \case
  0 -> nz
  n -> ns $ nFromNatural $ n - 1

nToNum :: Num a => Nat a -> a
nToNum f = f (+1) 0
