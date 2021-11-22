module HW1.T6
  ( epart
  , mcat
  ) where

import           Data.Foldable (fold, foldl')

mcat :: Monoid a => [Maybe a] -> a
mcat = fold . fold

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldl' adder (mempty, mempty)
 where
   adder :: (Monoid a, Monoid b) => (a, b) -> Either a b -> (a, b)
   adder (l, r) (Left a)  = (l <> a, r)
   adder (l, r) (Right a) = (l, r <> a)
