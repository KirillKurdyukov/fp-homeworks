module HW1.T6 (
 mcat,
 epart
) where

import Data.Either (lefts, rights)
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap (fromMaybe mempty)

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart l = (fold $ lefts l, fold $ rights l)
