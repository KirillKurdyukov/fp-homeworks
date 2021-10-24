module HW1.T4
  ( tfoldr
  , treeToList
  ) where

import HW1.T3 (Tree (..))

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ ini Leaf                        = ini
tfoldr f ini (Branch _ leftT val rightT) = tfoldr f (val `f` tfoldr f ini rightT) leftT

treeToList :: Tree a -> [a]    -- output list is sorted
treeToList = tfoldr (:) []
