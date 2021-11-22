{-# LANGUAGE LambdaCase #-}

module HW2.T4
  ( Expr (..)
  , Prim (..)
  , State (..)
  , eval
  , joinState
  , mapState
  , modifyState
  , wrapState
  ) where

import Control.Monad (ap)
import HW2.T1 (Annotated (..), mapAnnotated)

newtype State s a = S {runS :: s -> Annotated s a} 

mapState :: (a -> b) -> State s a -> State s b
mapState f m = S $ \s -> let a = runS m s in mapAnnotated f a

wrapState :: a -> State s a
wrapState a = S $ \s -> a :# s

joinState :: State s (State s a) -> State s a
joinState m = S $ \s -> let (newM :# newS) = runS m s in runS newM newS

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a =
    Add a a      -- (+)
  | Sub a a      -- (-)
  | Mul a a      -- (*)
  | Div a a      -- (/)
  | Abs a        -- abs
  | Sgn a        -- signum
  deriving Show 

data Expr = Val Double | Op (Prim Expr) deriving Show

instance Num Expr where
  x + y = Op (Add x y)
  x - y = Op (Sub x y)
  x * y = Op (Mul x y)

  fromInteger x = Val (fromInteger x)

  abs x = Op $ Abs x
  signum x = Op $ Sgn x

instance Fractional Expr where
  x / y = Op (Div x y)

  fromRational x = Val $ fromRational x

binary :: (Double -> Double -> Prim Double)
 -> (Double -> Double -> Double)
 -> Expr -> Expr -> State [Prim Double] Double
binary c f x y = do
  newX <- eval x
  newY <- eval y
  modifyState (c newX newY :)
  return $ newX `f` newY

unary :: (Double -> Prim Double)
 -> (Double -> Double)
 -> Expr -> State [Prim Double] Double
unary c f x = do
  newX <- eval x
  modifyState (c newX :)
  return $ f newX

eval :: Expr -> State [Prim Double] Double
eval = \case
  (Val v) -> return v
  (Op op) ->
    case op of
      (Add x y) -> binary Add (+) x y
      (Sub x y) -> binary Sub (-) x y
      (Mul x y) -> binary Mul (*) x y
      (Div x y) -> binary Div (/) x y
      (Abs x)   -> unary Abs abs x
      (Sgn x)   -> unary Sgn signum x



