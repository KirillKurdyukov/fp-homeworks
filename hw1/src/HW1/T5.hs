module HW1.T5 (
  splitOn,
  joinWith
) where

import Data.List.NonEmpty

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr adder ([] :| [])
 where
   adder x xs@(h :| t) = if x == sep then [] :| toList xs else (x : h) :| t

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep = foldr1 (\x xs -> x ++ [sep] ++ xs)
