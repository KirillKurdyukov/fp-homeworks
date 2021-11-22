module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import           Data.Function (fix)
import           GHC.Natural

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' = fix (\rec f l
  -> case l of
    []       -> []
    (x : xs) -> f x : rec f xs)

fib :: Natural -> Natural
fib = fix (\_ n -> helper 0 1 n)
   where
     helper :: Natural -> Natural -> Natural -> Natural
     helper = fix (\rec a b n
      -> if n == 0 then a else rec b (a + b) (n - 1))

fac :: Natural -> Natural
fac = fix (\rec n -> if n <= 1 then 1 else n * rec (n - 1))
