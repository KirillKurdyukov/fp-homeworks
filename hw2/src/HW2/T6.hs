{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module HW2.T6
  ( ParseError (..)
  , Parser (..)
  , pAbbr
  , pChar
  , pEof
  , parseExpr
  , runP
  ) where

import GHC.Natural

import Control.Applicative (Alternative (..), optional)
import Control.Monad (MonadPlus (..), mfilter, void)
import Data.Char (digitToInt, isDigit, isSpace, isUpper)
import Data.Scientific (scientific, toRealFloat)

import HW2.T1 (Annotated (..), Except (..))
import HW2.T4 (Expr (..), Prim (..))
import HW2.T5 (ExceptState (..))

newtype ParseError = ErrorAtPos Natural deriving Show
--data ExceptState e s a = ES {runES :: s -> Except e (Annotated s a)}
newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P p) str
  = case runES p (0, str) of
      (Error e) -> Error e
      (Success (a :# _)) -> Success a

{-
 1. What happens when the string is empty?
 Answer: runP pChar "" return Error (ErrorAtPos 0)

 2. How does the parser state changes when a symbol is consumed?
 Answer: return c :: Char and state changes pos++, and string is tail.
 <code>
  Success (c :# (pos + 1, cs))
 </code>
-}
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []       -> Error (ErrorAtPos pos)
    (c : cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \_ -> Error $ ErrorAtPos 0

instance Alternative Parser where
  empty = parseError
  (P p) <|> (P q) = P $ ES $ \s ->
    case runES p s of
      (Error _) -> runES q s
      x         -> x

instance MonadPlus Parser   -- No methods.

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
  case s of
    [] -> Success (() :# (pos, ""))
    _  -> Error $ ErrorAtPos pos

pAbbr :: Parser String
pAbbr = do
  abbr <- some (mfilter isUpper pChar)
  pEof
  pure abbr

{-
  Grammar LL(1)
  E -> TE'
  E'-> +TE' | -TE' | eps
  T -> FT'
  T'-> *FT' | /FT' | eps
  F -> n | (E)
-}

parseExpr :: String -> Except ParseError Expr
parseExpr = runP startParserMathExpr

startParserMathExpr :: Parser Expr
startParserMathExpr = do 
  res <- nonTerminalE
  skipWhiteSpace
  pEof
  return res 

getOp :: Char -> Prim a a  
getOp = \case
  '+' -> Add
  '-' -> Sub 
  '*' -> Mul 
  '/' -> Div 
  _   -> undefined
  
  
nonTerminalF :: Parser Expr
nonTerminalF = do
  skipWhiteSpace
  bracket <- getCMaybe (== '(')
  case bracket of
    (Just '(') -> do
      res <- nonTerminalE
      skipWhiteSpace
      op <- getCMaybe (== ')')
      case op of
        (Just ')') -> return res
        _          -> parseError
    _          -> parseDouble

nonTerminalE :: Parser Expr
nonTerminalE = do
  acc <- nonTerminalT
  nonTerminalE' acc

nonTerminalT :: Parser Expr
nonTerminalT = do
  acc <- nonTerminalF
  nonTerminalT' acc 

nonTerminalE' :: Expr -> Parser Expr
nonTerminalE' acc = flip (<|>) (return acc) $ do
  skipWhiteSpace
  op  <- getC (== '+') <|> getC (== '-')
  t   <- nonTerminalT
  nonTerminalE' $ getOp op acc t

nonTerminalT' :: Expr -> Parser Expr
nonTerminalT' acc = flip (<|>) (return acc) $ do
  skipWhiteSpace
  op <- getC (== '*') <|> getC (== '/')
  f  <- nonTerminalF
  nonTerminalT' getOp op acc f

skipWhiteSpace :: Parser ()
skipWhiteSpace = void $ many $ mfilter isSpace pChar

getC :: (Char -> Bool) -> Parser Char
getC p = mfilter p pChar

getCMaybe :: (Char -> Bool) -> Parser (Maybe Char)
getCMaybe p = optional $ getC p

parseDouble :: Parser Expr
parseDouble = do
  intPart <- parseDigit
  dot <- getCMaybe (== '.')
  case dot of
    (Just '.') -> do
      floatPart <- parseDigit
      return $ Val $ toRealFloat $ fromInteger (parseIntPart intPart) 
        + parseFloatPart floatPart (length floatPart)
    _ -> return $ Val $ fromInteger $ parseIntPart intPart

parseIntPart :: String -> Integer
parseIntPart = fromIntegral . foldl (\a x -> a * 10 + x) 0 . map digitToInt

parseFloatPart :: String -> Int -> Scientific
parseFloatPart str len = scientific (parseIntPart str) (-1 * len) 

parseDigit :: Parser String
parseDigit = some (mfilter isDigit pChar)

{-
Genius move

before:

nonTerminalT' :: Expr -> Parser Expr
nonTerminalT' acc = do
  skipWhiteSpace
  op <- getC (== '*') <|> getC (== '/')
  f  <- nonTerminalF
  newAcc  <- case op of
    '*' -> return $ Op $ Mul acc f
    '/' -> return $ Op $ Div acc f
    _   -> return acc
  nonTerminalT' newAcc <|> return newAcc
  
after:

nonTerminalT' :: Expr -> Parser Expr
nonTerminalT' acc = flip (<|>) (return acc) $ do
  skipWhiteSpace
  op <- getC (== '*') <|> getC (== '/')
  f  <- nonTerminalF
  nonTerminalT' getOp op acc f  
-}