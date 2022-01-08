{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module HW3.Base where

import           Codec.Serialise (Serialise)
import           Data.ByteString (ByteString)
import           Data.Map        (Map)
import           Data.Sequence   (Seq)
import           Data.Text       (Text)
import           Data.Time       (UTCTime)
import           GHC.Generics    (Generic)

data HiFun
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Eq, Ord, Generic)

instance Serialise HiFun

data HiValue
  = HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Eq, Ord, Show, Generic)

instance Serialise HiValue

data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show)

data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show)

data HiAction
  = HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Eq, Ord, Show, Generic)

instance Serialise HiAction

data Arity
  = Unary
  | Binary
  | BinaryLazy
  | Triple
  | Many

getArity :: HiFun -> Arity
getArity = \case
  HiFunDiv            -> Binary
  HiFunMul            -> Binary
  HiFunAdd            -> Binary
  HiFunSub            -> Binary
  HiFunNot            -> Unary
  HiFunAnd            -> BinaryLazy
  HiFunOr             -> BinaryLazy
  HiFunLessThan       -> Binary
  HiFunGreaterThan    -> Binary
  HiFunEquals         -> Binary
  HiFunNotLessThan    -> Binary
  HiFunNotGreaterThan -> Binary
  HiFunNotEquals      -> Binary
  HiFunIf             -> Triple
  HiFunLength         -> Unary
  HiFunToUpper        -> Unary
  HiFunToLower        -> Unary
  HiFunReverse        -> Unary
  HiFunTrim           -> Unary
  HiFunList           -> Many
  HiFunRange          -> Binary
  HiFunFold           -> Binary
  HiFunPackBytes      -> Unary
  HiFunUnpackBytes    -> Unary
  HiFunEncodeUtf8     -> Unary
  HiFunDecodeUtf8     -> Unary
  HiFunZip            -> Unary
  HiFunUnzip          -> Unary
  HiFunSerialise      -> Unary
  HiFunDeserialise    -> Unary
  HiFunRead           -> Unary
  HiFunWrite          -> Binary
  HiFunMkDir          -> Unary
  HiFunChDir          -> Unary
  HiFunParseTime      -> Unary
  HiFunRand           -> Binary
  HiFunEcho           -> Unary
  HiFunCount          -> Unary
  HiFunKeys           -> Unary
  HiFunValues         -> Unary
  HiFunInvert         -> Unary

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

instance Show HiFun where
   show = \case
    HiFunDiv            -> "div"
    HiFunAdd            -> "add"
    HiFunMul            -> "mul"
    HiFunSub            -> "sub"
    HiFunNot            -> "not"
    HiFunAnd            -> "and"
    HiFunOr             -> "or"
    HiFunLessThan       -> "less-than"
    HiFunGreaterThan    -> "greater-than"
    HiFunEquals         -> "equals"
    HiFunNotLessThan    -> "not-less-than"
    HiFunNotGreaterThan -> "not-greater-than"
    HiFunNotEquals      -> "not-equals"
    HiFunIf             -> "if"
    HiFunLength         -> "length"
    HiFunToUpper        -> "to-upper"
    HiFunToLower        -> "to-lower"
    HiFunReverse        -> "reverse"
    HiFunTrim           -> "trim"
    HiFunList           -> "list"
    HiFunRange          -> "range"
    HiFunFold           -> "fold"
    HiFunPackBytes      -> "pack-bytes"
    HiFunUnpackBytes    -> "unpack-bytes"
    HiFunEncodeUtf8     -> "encode-utf8"
    HiFunDecodeUtf8     -> "decode-utf8"
    HiFunZip            -> "zip"
    HiFunUnzip          -> "unzip"
    HiFunSerialise      -> "serialise"
    HiFunDeserialise    -> "deserialise"
    HiFunRead           -> "read"
    HiFunWrite          -> "write"
    HiFunMkDir          -> "mkdir"
    HiFunChDir          -> "cd"
    HiFunParseTime      -> "parse-time"
    HiFunRand           -> "rand"
    HiFunEcho           -> "echo"
    HiFunCount          -> "count"
    HiFunKeys           -> "keys"
    HiFunValues         -> "values"
    HiFunInvert         -> "invert"