module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import           Data.Function (fix)
import           GHC.Natural

repeat' :: a -> [a]
repeat' = fix (\rec x -> x : rec x)

map' :: (a -> b) -> [a] -> [b]
map' = fix (\rec f l
  -> case l of
    []       -> []
    (x : xs) -> f x : rec f xs)

fib :: Natural -> Natural
fib = fix (\rec n -> if n == 0 then 0
   else if n == 1 then 1
   else rec (n - 1) + rec (n - 2))

fac :: Natural -> Natural
fac = fix (\rec n -> if n <= 1 then 1 else n * rec (n - 1))
