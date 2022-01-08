{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser where

import           Control.Monad.Combinators.Expr
import           Data.ByteString                hiding (count, head,
                                                 intercalate)
import           Data.Char                      (isAlpha, isAlphaNum)
import           Data.List                      (intercalate)
import qualified Data.Text                      as T (Text, pack)
import           Data.Void                      (Void)
import           Data.Word                      (Word8)
import           HW3.Base
import           Numeric                        (readHex)
import           Text.Megaparsec                hiding (parse)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space *> hiExpr <* eof) ""

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: String -> String -> Parser a -> Parser a
parens open close = between (symbol open) (symbol close)

parensBracket :: Parser a -> Parser a
parensBracket = parens "(" ")"

parensSquare :: Parser a -> Parser a
parensSquare = parens "[" "]"

parensLattice :: Parser a -> Parser a
parensLattice = parens "[# " "#]"

parensJson :: Parser a -> Parser a
parensJson = parens "{" "}"

parensValue :: Parser a -> Parser a
parensValue p = symbol "(" *> parensValue p <* symbol ")" <|> p

comma :: Parser String
comma = symbol ","

args :: Parser [HiExpr]
args = hiExpr `sepBy` comma

pair :: Parser (HiExpr, HiExpr)
pair = (,) <$> (hiExpr <* symbol ":") <*> hiExpr

json :: Parser [(HiExpr, HiExpr)]
json = parensJson $ pair `sepBy` comma

bytes :: Parser [Word8]
bytes = many byte

byte :: Parser Word8
byte = lexeme $ fst . head . readHex <$> count 2 hexDigitChar <* space1

rational :: Parser Rational
rational = L.signed sc $ lexeme $ toRational <$> L.scientific

bool :: Parser Bool
bool = True <$ symbol "true" <|> False <$ symbol "false"

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

list :: Parser HiExpr
list = do
  e <- HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> parensSquare args
  recExpr e

byteString :: Parser ByteString
byteString = pack <$> parensLattice bytes

indeficator :: Parser String
indeficator = intercalate "-" <$>
 ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy` char '-'

arg :: Parser T.Text
arg = lexeme $ T.pack <$> indeficator

afterDot :: Parser [HiExpr]
afterDot = return . HiExprValue . HiValueString <$> arg

dot :: Parser a -> Parser a
dot p = char '.' *> p

hiValue :: Parser HiExpr
hiValue =
  HiExprValue
    <$> choice
      [ HiValueFunction
          <$> choice
            [ HiFunDiv <$ symbol "div",
              HiFunMul <$ symbol "mul",
              HiFunAdd <$ symbol "add",
              HiFunSub <$ symbol "sub",
              HiFunNotGreaterThan <$ symbol "not-greater-than",
              HiFunNotEquals <$ symbol "not-equals",
              HiFunNotLessThan <$ symbol "not-less-than",
              HiFunNot <$ symbol "not",
              HiFunAnd <$ symbol "and",
              HiFunOr <$ symbol "or",
              HiFunLessThan <$ symbol "less-than",
              HiFunGreaterThan <$ symbol "greater-than",
              HiFunEquals <$ symbol "equals",
              HiFunIf <$ symbol "if",
              HiFunLength <$ symbol "length",
              HiFunToUpper <$ symbol "to-upper",
              HiFunToLower <$ symbol "to-lower",
              HiFunReverse <$ symbol "reverse",
              HiFunTrim <$ symbol "trim",
              HiFunList <$ symbol "list",
              HiFunRange <$ symbol "range",
              HiFunFold <$ symbol "fold",
              HiFunPackBytes <$ symbol "pack-bytes",
              HiFunUnpackBytes <$ symbol "unpack-bytes",
              HiFunZip <$ symbol "zip",
              HiFunUnzip <$ symbol "unzip",
              HiFunEncodeUtf8 <$ symbol "encode-utf8",
              HiFunDecodeUtf8 <$ symbol "decode-utf8",
              HiFunSerialise <$ symbol "serialise",
              HiFunDeserialise <$ symbol "deserialise",
              HiFunRead <$ symbol "read",
              HiFunWrite <$ symbol "write",
              HiFunMkDir <$ symbol "mkdir",
              HiFunChDir <$ symbol "cd",
              HiFunParseTime <$ symbol "parse-time",
              HiFunRand <$ symbol "rand",
              HiFunEcho <$ symbol "echo",
              HiFunCount <$ symbol "count",
              HiFunKeys <$ symbol "keys",
              HiFunValues <$ symbol "values",
              HiFunInvert <$ symbol "invert"
            ],
        HiValueNumber <$> rational,
        HiValueBool <$> bool,
        HiValueNull <$ symbol "null",
        HiValueString . T.pack <$> stringLiteral,
        HiValueBytes <$> byteString,
        HiValueAction HiActionCwd <$ symbol "cwd",
        HiValueAction HiActionNow <$ symbol "now"
      ]
    <|> HiExprDict <$> json
    <|> parensBracket hiExpr

argsHi :: Parser [HiExpr]
argsHi = parensBracket args <|> dot afterDot

hiFun :: Parser HiExpr
hiFun = do
  val <- hiValue
  x <- (HiExprApply val <$> argsHi) <|> pure val
  recExpr x

hiExpr :: Parser HiExpr
hiExpr =
  makeExprParser
    ( lexeme
        ( choice
            [ hiFun,
              list
            ]
        )
    )
    operatorTable

recExpr :: HiExpr -> Parser HiExpr
recExpr e = flip (<|>) (return e) $ do
  res <- HiExprApply e <$> argsHi
  recExpr res

infixL :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
infixL name f = InfixL (f <$ symbol name)

infixR :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
infixR name f = InfixR (f <$ symbol name)

infixN :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
infixN name f = InfixN (f <$ symbol name)

createApplyBinary :: HiFun -> HiExpr -> HiExpr -> HiExpr
createApplyBinary fun a b = HiExprApply (HiExprValue (HiValueFunction fun)) [a, b]

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ Postfix $ HiExprRun <$ symbol "!"
    ],
    [ infixL "*" $ createApplyBinary HiFunMul,
      InfixL $ createApplyBinary HiFunDiv <$ (lexeme . try) (string "/" <* notFollowedBy (char '='))
    ],
    [ infixL "+" $ createApplyBinary HiFunAdd,
      infixL "-" $ createApplyBinary HiFunSub
    ],
    [ infixN ">=" $ createApplyBinary HiFunNotLessThan,
      infixN "<=" $ createApplyBinary HiFunNotGreaterThan,
      infixN "<" $ createApplyBinary HiFunLessThan,
      infixN ">" $ createApplyBinary HiFunGreaterThan,
      infixN "==" $ createApplyBinary HiFunEquals,
      infixN "/=" $ createApplyBinary HiFunNotEquals
    ],
    [ infixR "&&" $ createApplyBinary HiFunAnd
    ],
    [ infixR "||" $ createApplyBinary HiFunOr
    ]
  ]
