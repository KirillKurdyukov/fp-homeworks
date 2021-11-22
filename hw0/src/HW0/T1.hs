{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TypeOperators      #-}


module HW0.T1
  ( type (<->) (..)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

data a <-> b = Iso (a -> b) (b -> a)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib = \case
  Left a       -> (Left a, Left a)
  Right (b, c) -> (Right b, Right c)

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso (\(a, (b, c)) -> ((a, b), c)) (\((a, b), c) -> (a, (b, c)))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = let
  f :: Either a (Either b c) -> Either (Either a b) c
  f = \case
    Left a          -> Left $ Left a
    Right (Left b)  -> Left $ Right b
    Right (Right c) -> Right c
  g :: Either (Either a b) c -> Either a (Either b c)
  g = \case
    Left (Left a)  -> Left a
    Left (Right b) -> Right $ Left b
    Right c        -> Right $ Right c
  in Iso f g
