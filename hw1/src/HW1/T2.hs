module HW1.T2
  (
    N (..)
  , nplus
  , nmulti
  , nsub
  , ncmp
  , nFromNatural
  , nToNum
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Data.Maybe
import Numeric.Natural

data N = Z | S N --Арифметика Пеано
  deriving(Show)

nplus :: N -> N -> N -- addition
nplus Z b     = b
nplus (S c) b = nplus c $ S b

nmulti :: N -> N -> N-- multiplication
nmulti _ Z     = Z
nmulti a (S c) = nplus a $ nmulti a c

nsub :: N -> N -> Maybe N -- subtraction
nsub Z (S _)     = Nothing
nsub a Z         = Just a
nsub (S a) (S b) = nsub a b

ncmp :: N -> N -> Ordering -- comparison
ncmp a b =
  case nsub a b of
    Nothing  -> LT
    (Just Z) -> EQ
    (Just _) -> GT

nFromNatural :: Natural -> N
nFromNatural a =
  let
    helper :: Natural -> N -> N
    helper 0 n   = n
    helper acc n = helper (acc - 1) $ S n
   in helper a Z

nToNum :: Num a => N -> a
nToNum n =
  let
    helper :: Num a => N -> Natural -> a
    helper Z acc      = fromIntegral acc
    helper (S n') acc = helper n' (acc + 1)
   in helper n 0

-- Advanced modification
nEven :: N -> Bool
nEven Z         = True
nEven (S Z)     = False
nEven (S (S n)) = nEven n

nOdd :: N -> Bool
nOdd = not . nEven

ndiv :: N -> N -> N -- integer division
ndiv _ Z = error "Divide by zero!"
ndiv a b =
  let
    helper :: N -> N -> N -> N
    helper a' b' res =
      case nsub a' b' of
        Nothing -> res
        Just c  -> helper c b' $ S res
   in helper a b Z

nmod :: N -> N -> N
nmod a b = fromJust $ nsub a $ nmulti b $ ndiv a b
