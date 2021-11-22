{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

{-
 wrap <=> return from Monad
 dist <=>
  <code>
   x <- m1
   y <- m2
   return (x, y)
  </code>
-}

module HW2.T2
  ( distAnnotated
  , distExcept
  , distFun
  , distList
  , distOption
  , distPair
  , distPrioritised
  , distQuad
  , distStream
  , wrapAnnotated
  , wrapExcept
  , wrapFun
  , wrapList
  , wrapOption
  , wrapPair
  , wrapPrioritised
  , wrapQuad
  , wrapStream
  ) where

import           HW2.T1 (Annotated (..), Except (..), Fun (..), List (..),
                         Option (..), Pair (..), Prioritised (..), Quad (..),
                         Stream (..), mapList, mapPrioritised)

distOption :: (Option a, Option b) -> Option (a, b)
distOption = \case
 (None, _)        -> None
 (_, None)        -> None
 (Some x, Some y) -> Some (x, y)

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P x y, P u v) = P (x, u) (y, v)

wrapPair :: a -> Pair a
wrapPair x = P x x

distQuad :: (Quad a, Quad b) -> Quad(a, b)
distQuad (Q x1 x2 x3 x4, Q y1 y2 y3 y4) = Q (x1, y1) (x2, y2) (x3, y3) (x4, y4)

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a1 :# e1, a2 :# e2) = (a1, a2) :# e1 <> e2

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated = (:# mempty)

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept = \case
 (Error e, _)           -> Error e
 (_, Error e)           -> Error e
 (Success a, Success b) -> Success (a, b)

wrapExcept :: a -> Except e a
wrapExcept = Success

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised = \case
 (High a, High b)   -> High (a, b)
 (High a, Medium b) -> High (a, b)
 (x, Low b)         -> mapPrioritised (, b) x
 (Low a, x)         -> mapPrioritised (a, ) x
 (Medium a, x)      -> mapPrioritised (a, ) x

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> ax, b :> bs) = (a, b) :> distStream (ax, bs)

wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

distList :: (List a, List b) -> List (a, b)
distList = \case
 (Nil, _)      -> Nil
 (x :. xs, l2) -> mapList (x, ) l2 <> curry distList xs l2

wrapList :: a -> List a
wrapList a = a :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f1, F f2) = F (\x -> (f1 x, f2 x))

wrapFun :: a -> Fun i a
wrapFun a = F $ \_ -> a
