module HW0.T2
  ( Not
  , doubleNeg
  , reduceTripleNeg
  ) where

import           Data.Function ((&))
import           Data.Void

type Not a = a -> Void

-- a -> (a -> Void) -> Void
doubleNeg :: a -> Not (Not a)
doubleNeg = (&)

-- Not (Not (Not a)) -> a -> Void
-- ((Not (Not a)) -> Void) -> a -> Void
-- (((a -> Void) -> Void) -> Void) -> a -> Void
reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg f a = f $ doubleNeg a

