module HW1.T3
  ( Tree (..)
  , tFromList
  , tdepth
  , tinsert
  , tmember
  , tsize
  ) where

import Data.Foldable (foldl')
import Data.Function (on)

-- AVL tree
data Tree a
  = Leaf
  | Branch (Int, Int) (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int -- Size of the tree, O(1)
tsize Leaf                     = 0
tsize (Branch (size, _) _ _ _) = size

tdepth :: Tree a -> Int -- Depth of the tree, O(1)
tdepth Leaf                      = 0
tdepth (Branch (_, depth) _ _ _) = depth

tmember :: Ord a => a -> Tree a -> Bool -- Check if the element is in the tree, О(log n)
tmember _ Leaf = False
tmember el (Branch _ leftT val rightT) =
  case compare el val of
    EQ -> True
    LT -> tmember el leftT
    GT -> tmember el rightT

balanceFactor :: Tree a -> Int
balanceFactor Leaf                      = 0
balanceFactor (Branch _ leftT _ rightT) = tdepth rightT - tdepth leftT

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch leftT v rightT =
  let
    newSize = tsize leftT + tsize rightT + 1
    newDepth = (max `on` tdepth) leftT rightT + 1
   in Branch (newSize, newDepth) leftT v rightT

rotateRight :: Tree a -> Tree a
rotateRight Leaf                                   = Leaf
rotateRight (Branch _ Leaf _ _)                    = undefined
rotateRight (Branch _ (Branch _ ll x lr) y rightT) = mkBranch ll x $ mkBranch lr y rightT

rotateLeft :: Tree a -> Tree a
rotateLeft Leaf                                  = Leaf
rotateLeft (Branch _ _ _ Leaf)                   = undefined
rotateLeft (Branch _ leftT x (Branch _ rl y rr)) = mkBranch (mkBranch leftT x rl) y rr

balanceTree :: Tree a -> Tree a
balanceTree Leaf = Leaf
balanceTree tree@(Branch _ leftT val rightT) =
  case balanceFactor tree of
    2 ->
      if balanceFactor rightT < 0
        then rotateLeft $ mkBranch leftT val $ rotateRight rightT
        else rotateLeft tree
    -2 ->
      if balanceFactor leftT > 0
        then rotateRight $ mkBranch (rotateLeft leftT) val rightT
        else rotateRight tree
    _ -> tree

tinsert :: Ord a => a -> Tree a -> Tree a -- Insert an element into tree, O(log n)
tinsert el Leaf = Branch (1, 1) Leaf el Leaf
tinsert el tree@(Branch _ leftT val rightT) =
  case compare el val of
    EQ -> tree
    LT -> balanceTree $ mkBranch (tinsert el leftT) val rightT
    GT -> balanceTree $ mkBranch leftT val $ tinsert el rightT

tFromList :: Ord a => [a] -> Tree a -- Build a tree from a list, O(n log n)
tFromList = foldl' (flip tinsert) Leaf
