module HW1.T6
  ( epart
  , mcat
  ) where

import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap (fromMaybe mempty)

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldl' adder (mempty, mempty)
 where
   adder :: (Monoid a, Monoid b) => (a, b) -> Either a b -> (a, b)
   adder (l, r) (Left a)  = (l <> a, r)
   adder (l, r) (Right a) = (l, r <> a)
