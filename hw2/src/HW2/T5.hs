{-# LANGUAGE LambdaCase #-}

module HW2.T5
  ( EvaluationError (..)
  , ExceptState (..)
  , eval
  , joinExceptState
  , mapExceptState
  , modifyExceptState
  , wrapExceptState
  ) where

import Control.Monad (ap)

import HW2.T1 (Annotated (..), Except (..), mapAnnotated)
import HW2.T4 (Expr (..), Prim (..))

data ExceptState e s a = ES {runES :: s -> Except e (Annotated s a)}

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f m = ES $ \s ->
 case runES m s of
    (Error e)   -> Error e
    (Success a) -> Success $ mapAnnotated f a

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success (a :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState m = ES $ \s ->
 case runES m s of
   (Error e)                -> Error e
   (Success (newM :# newS)) -> runES newM newS

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success (() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero

binary :: (Double -> Double -> Prim Double)
 -> (Double -> Double -> Double)
 -> Expr -> Expr -> ExceptState EvaluationError [Prim Double] Double
binary c f x y = do
  newX <- eval x
  newY <- eval y
  modifyExceptState (c newX newY :)
  return $ newX `f` newY

unary :: (Double -> Prim Double)
 -> (Double -> Double)
 -> Expr -> ExceptState EvaluationError [Prim Double] Double
unary c f x = do
  newX <- eval x
  modifyExceptState (c newX :)
  return $ f newX

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval = \case
  (Val v) -> return v
  (Op op) ->
    case op of
      (Add x y) -> binary Add (+) x y
      (Sub x y) -> binary Sub (-) x y
      (Mul x y) -> binary Mul (*) x y
      (Div x y) -> do
        newX <- eval x
        newY <- eval y
        if newY == 0
        then throwExceptState DivideByZero
        else do
          modifyExceptState (Div newX newY :)
          return $ newX / newY
      (Abs x) -> unary Abs abs x
      (Sgn x) -> unary Sgn signum x
