{-# LANGUAGE LambdaCase #-}
-- T3 about function join from Control.Monad
module HW2.T3
  ( joinAnnotated
  , joinExcept
  , joinFun
  , joinList
  , joinOption
  ) where

import HW2.T1 (Annotated (..), Except (..), Fun (..), List (..), Option (..))

joinOption :: Option (Option a) -> Option a
joinOption = \case
 (Some x) -> x
 _        -> None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept = \case
 (Error e)   -> Error e
 (Success x) -> x

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e1) :# e2) = a :# e2 <> e1

joinList :: List (List a) -> List a
joinList = \case
 Nil     -> Nil
 x :. xs -> x <> joinList xs

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F $ \i -> let (F newF) = f i in newF i
