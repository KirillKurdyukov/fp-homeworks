{-# LANGUAGE LambdaCase #-}

module HW3.Evaluator where

import Codec.Compression.Zlib
import Codec.Serialise
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import qualified Data.ByteString as B (pack, unpack)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (toList)
import qualified Data.Map as M (Map, elems, fromList, fromListWith, keys, lookup, map, toList)
import Data.Ratio (denominator)
import Data.Semigroup (stimes)
import Data.Sequence (fromList, reverse)
import qualified Data.Text as T (Text, length, pack, reverse, singleton, strip, toLower, toUpper,
                                 unpack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Word
import HW3.Base
import Prelude hiding (reverse)
import Text.Read (readMaybe)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalHiExpr

evalHiExpr :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalHiExpr = \case
  (HiExprValue val) -> return val
  (HiExprDict assoc) -> HiValueDict . M.fromList <$> mapM (\(k, v) -> do
    k' <- evalHiExpr k
    v' <- evalHiExpr v
    return (k', v')) assoc
  (HiExprRun r) -> do
     r' <- evalHiExpr r
     case r' of
       HiValueAction action -> lift (runAction action)
       _                    -> throwE HiErrorInvalidArgument
  (HiExprApply fun args) -> do
    f <- evalHiExpr fun
    case f of
      (HiValueFunction f') -> case getArity f' of
        Unary      -> unary f' args
        Binary     -> binary f' args
        BinaryLazy -> binaryLazy f' args
        Triple     -> triple f' args
        Many       -> many f' args
      (HiValueString s) -> indexHi (T.unpack s) args $ return . HiValueString . T.pack
      (HiValueList list) -> 
        if length args > 1 
          then indexHi (toList list) args $ return . HiValueList . fromList
          else indexHi (toList list) args $ return . head
      (HiValueBytes bytes) ->
        if length args > 1
          then indexHi (B.unpack bytes) args $ return . HiValueBytes . B.pack
          else indexHi (B.unpack bytes) args $ return . HiValueNumber . fromIntegral . head
      (HiValueDict assoc) -> get assoc args
      _ -> throwE HiErrorInvalidFunction

get :: HiMonad m => M.Map HiValue HiValue -> [HiExpr] -> ExceptT HiError m HiValue
get assoc [e] = do
  x <- evalHiExpr e
  case M.lookup x assoc of
    Just val -> return val
    Nothing  -> return HiValueNull
get _ _ = throwE HiErrorArityMismatch

unary ::
  HiMonad m =>
  HiFun ->
  [HiExpr] ->
  ExceptT HiError m HiValue
unary f [e] = do
  x <- evalHiExpr e
  case (f, x) of
    (HiFunNot, HiValueBool x') -> return $ HiValueBool $ not x'
    (HiFunLength, HiValueString str) -> return $ HiValueNumber $ fromIntegral $ T.length str
    (HiFunLength, HiValueList list) -> return $ HiValueNumber $ fromIntegral $ length list
    (HiFunToUpper, HiValueString str) -> return $ HiValueString $ T.toUpper str
    (HiFunToLower, HiValueString str) -> return $ HiValueString $ T.toLower str
    (HiFunReverse, HiValueString str) -> return $ HiValueString $ T.reverse str
    (HiFunReverse, HiValueList list) -> return $ HiValueList $ reverse list
    (HiFunTrim, HiValueString str) -> return $ HiValueString $ T.strip str
    (HiFunPackBytes, HiValueList list) -> HiValueBytes . B.pack <$> mapM mapByte (toList list)
    (HiFunUnpackBytes, HiValueBytes bytes) ->
      return $
        HiValueList $
          fromList $
            map (HiValueNumber . fromIntegral) $ B.unpack bytes
    (HiFunZip, HiValueBytes bytes) ->
      return $
        HiValueBytes $
          toStrict $ compressWith defaultCompressParams {compressLevel = bestCompression} $ fromStrict bytes
    (HiFunUnzip, HiValueBytes bytes) ->
      return $
        HiValueBytes $
          toStrict $ decompressWith defaultDecompressParams $ fromStrict bytes
    (HiFunSerialise, x') -> return $ HiValueBytes $ toStrict $ serialise x'
    (HiFunDeserialise, HiValueBytes bytes) -> return $ HiValueBytes $ deserialise $ fromStrict bytes
    (HiFunEncodeUtf8, HiValueString str) -> return $ HiValueBytes $ encodeUtf8 str
    (HiFunDecodeUtf8, HiValueBytes bytes) -> case decodeUtf8' bytes of
      Right res -> return $ HiValueString res
      Left _    -> return HiValueNull
    (HiFunRead, HiValueString path) -> createHiAction HiActionRead path
    (HiFunMkDir, HiValueString path) -> createHiAction HiActionMkDir path
    (HiFunChDir, HiValueString path) -> createHiAction HiActionChDir path
    (HiFunParseTime, HiValueString time) -> case (readMaybe (T.unpack time) :: Maybe UTCTime) of
      Nothing      -> return HiValueNull
      Just utctime -> return $ HiValueTime utctime
    (HiFunEcho, HiValueString path) -> return $ HiValueAction $ HiActionEcho path
    (HiFunKeys, HiValueDict assoc) -> return $ HiValueList $ fromList $ M.keys assoc
    (HiFunValues, HiValueDict assoc) -> return $ HiValueList $ fromList $ M.elems assoc
    (HiFunCount, HiValueList list) -> return $ HiValueDict $ countElems $ toList list
    (HiFunCount, HiValueString list) -> return $ HiValueDict
      $ countElems $ map (HiValueString . T.singleton) $ T.unpack list
    (HiFunCount, HiValueBytes list) -> return $ HiValueDict
      $ countElems $ map (HiValueNumber . fromIntegral) $ B.unpack list
    (HiFunInvert, HiValueDict assoc) -> return $ HiValueDict $ M.map (HiValueList . fromList) $ invert assoc
    _ -> throwE HiErrorInvalidArgument
unary _ _ = throwE HiErrorArityMismatch

binary ::
  HiMonad m =>
  HiFun ->
  [HiExpr] ->
  ExceptT HiError m HiValue
binary fun [e1, e2] = do
  x <- evalHiExpr e1
  y <- evalHiExpr e2
  case (fun, x, y) of
    (HiFunDiv, HiValueNumber _, HiValueNumber 0) -> throwE HiErrorDivideByZero
    (HiFunDiv, HiValueNumber x', HiValueNumber y') -> return $ HiValueNumber $ x' / y'
    (HiFunDiv, HiValueString x', HiValueString y') -> return $ HiValueString $ x' <> T.pack "/" <> y'
    (HiFunMul, HiValueNumber x', HiValueNumber y') -> return $ HiValueNumber $ x' * y'
    (HiFunMul, HiValueNumber x', HiValueString y') -> replicateHi (return . HiValueString) x' y'
    (HiFunMul, HiValueString x', HiValueNumber y') -> replicateHi (return . HiValueString) y' x'
    (HiFunMul, HiValueNumber x', HiValueList y') -> replicateHi (return . HiValueList) x' y'
    (HiFunMul, HiValueList x', HiValueNumber y') -> replicateHi (return . HiValueList) y' x'
    (HiFunMul, HiValueNumber x', HiValueBytes y') -> replicateHi (return . HiValueBytes) x' y'
    (HiFunMul, HiValueBytes x', HiValueNumber y') -> replicateHi (return . HiValueBytes) y' x'
    (HiFunAdd, HiValueNumber x', HiValueNumber y') -> return $ HiValueNumber $ x' + y'
    (HiFunAdd, HiValueString x', HiValueString y') -> return $ HiValueString $ x' <> y'
    (HiFunAdd, HiValueList x', HiValueList y') -> return $ HiValueList $ x' <> y'
    (HiFunAdd, HiValueBytes x', HiValueBytes y') -> return $ HiValueBytes $ x' <> y'
    (HiFunAdd, HiValueTime time, HiValueNumber diff) -> return $ HiValueTime $ addUTCTime (fromRational diff) time
    (HiFunAdd, HiValueNumber diff, HiValueTime time) -> return $ HiValueTime $ addUTCTime (fromRational diff) time
    (HiFunSub, HiValueNumber x', HiValueNumber y') -> return $ HiValueNumber $ x' - y'
    (HiFunSub, HiValueTime t1, HiValueTime t2) -> return $ HiValueNumber $ toRational $ diffUTCTime t1 t2
    (HiFunLessThan, HiValueBool _, HiValueNumber _) -> return $ HiValueBool True
    (HiFunLessThan, HiValueNumber _, HiValueBool _) -> return $ HiValueBool False
    (HiFunLessThan, x', y') -> return $ HiValueBool $ x' < y'
    (HiFunEquals, x', y') -> return $ HiValueBool $ x' == y'
    (HiFunGreaterThan, _, _) -> binary HiFunLessThan [e2, e1]
    (HiFunNotLessThan, _, _) -> reverseCompare HiFunLessThan [e1, e2]
    (HiFunNotGreaterThan, _, _) -> reverseCompare HiFunGreaterThan [e1, e2]
    (HiFunNotEquals, _, _) -> reverseCompare HiFunEquals [e1, e2]
    (HiFunRange, HiValueNumber x', HiValueNumber y') -> return $ HiValueList $ fromList $ map HiValueNumber [x' .. y']
    (HiFunFold, HiValueFunction f, HiValueList list) -> case getArity f of
      Binary ->
        if null list
          then return HiValueNull
          else
            let l = map pure $ toList list
             in foldl1
                  ( \a b -> do
                      a' <- a
                      b' <- b
                      binary f [HiExprValue a', HiExprValue b']
                  )
                  l
      _ -> throwE HiErrorInvalidArgument
    (HiFunWrite, HiValueString path, HiValueString bytes) ->
      return $ HiValueAction $ HiActionWrite (T.unpack path) (encodeUtf8 bytes)
    (HiFunRand, HiValueNumber i, HiValueNumber j) ->
      if denominator i == 1 && denominator j == 1
      then return $ HiValueAction $ HiActionRand (truncate i) (truncate j)
      else return HiValueNull
    _ -> throwE HiErrorInvalidArgument
binary _ _ = throwE HiErrorArityMismatch

binaryLazy ::
 HiMonad m =>
 HiFun ->
 [HiExpr] ->
 ExceptT HiError m HiValue
binaryLazy fun [e1, e2] = do
  x <- evalHiExpr e1
  case (fun, x) of
    (HiFunAnd, HiValueNull)       -> return HiValueNull
    (HiFunAnd, HiValueBool True)  -> evalHiExpr e2
    (HiFunAnd, HiValueBool False) -> return x
    (HiFunAnd, _)                 -> evalHiExpr e2
    (HiFunOr, HiValueNull)        -> evalHiExpr e2
    (HiFunOr, HiValueBool False)  -> evalHiExpr e2
    (HiFunOr, _)                  -> return x
    _                             -> throwE HiErrorInvalidArgument
binaryLazy _ _ = throwE HiErrorArityMismatch

triple ::
  HiMonad m =>
  HiFun ->
  [HiExpr] ->
  ExceptT HiError m HiValue
triple fun [e1, e2, e3] = do
  x <- evalHiExpr e1
  case (fun, x) of
    (HiFunIf, HiValueBool True)  -> evalHiExpr e2
    (HiFunIf, HiValueBool False) -> evalHiExpr e3
    _                            -> throwE HiErrorInvalidArgument
triple _ _ = throwE HiErrorArityMismatch

reverseCompare ::
  HiMonad m =>
  HiFun ->
  [HiExpr] ->
  ExceptT HiError m HiValue
reverseCompare f es = do
  r <- binary f es
  case r of
    (HiValueBool res) -> return $ HiValueBool $ not res
    _                 -> throwE HiErrorInvalidArgument

indexHi ::
  HiMonad m =>
  [a] ->
  [HiExpr] ->
  ([a] -> ExceptT HiError m HiValue) ->
  ExceptT HiError m HiValue
indexHi as [e1] to = do
  i <- evalHiExpr e1
  case i of
    (HiValueNumber i') -> do
      _ <- isInteger i'
      if i' >= 0 && i' < fromIntegral (length as)
        then to [as !! truncate i']
        else return HiValueNull
    _ -> throwE HiErrorInvalidArgument
indexHi as [e1, e2] to = do
  let size = length as
  i <- evalHiExpr e1
  j <- evalHiExpr e2
  case (i, j) of
    (HiValueNumber i', HiValueNumber j') -> do
      _ <- isInteger i'
      _ <- isInteger j'
      to $ slice i' j' as
    (HiValueNumber i', HiValueNull)      -> do
      _ <- isInteger i'
      to $ slice i' (fromIntegral size) as
    (HiValueNull, HiValueNumber j')      -> do
      _ <- isInteger j'
      to $ slice 0 j' as
    (HiValueNull, HiValueNull)           -> to $ slice 0 (fromIntegral size) as
    _                                    -> throwE HiErrorInvalidArgument
indexHi _ _ _ = throwE HiErrorArityMismatch

isInteger :: HiMonad m => Rational -> ExceptT HiError m HiValue
isInteger r = if denominator r == 1
  then return (HiValueNumber r)
  else throwE HiErrorInvalidArgument

replicateHi :: (HiMonad m, Semigroup a) =>
  (a -> ExceptT HiError m HiValue) ->
  Rational -> a
  -> ExceptT HiError m HiValue
replicateHi f r
  | r > 0 && denominator r == 1 = f . stimes (truncate r :: Int)
  | otherwise = \_ -> throwE HiErrorInvalidArgument

slice :: Rational -> Rational -> [a] -> [a]
slice i j str =
  let size = length str
      i' = if i < 0 then size - abs (truncate i) else truncate i
      j' = if j < 0 then abs (truncate j) else size - truncate j
   in (drop i' . take (size - j')) str

many ::
  HiMonad m =>
  HiFun ->
  [HiExpr] ->
  ExceptT HiError m HiValue
many f args =
  case f of
    HiFunList -> HiValueList . fromList <$> mapM evalHiExpr args
    _         -> throwE HiErrorArityMismatch --impossible outcome

mapByte :: HiMonad m => HiValue -> ExceptT HiError m Word8
mapByte = \case
  HiValueNumber val ->
    if val >= 0 && val <= 255 && denominator val == 1
      then return $ fromInteger $ truncate val
      else throwE HiErrorInvalidArgument
  _ -> throwE HiErrorInvalidArgument

createHiAction :: Monad m => (FilePath -> HiAction) -> T.Text -> ExceptT HiError m HiValue
createHiAction to path = return $ HiValueAction $ to $ T.unpack path

countElems :: (Ord a) => [a] -> M.Map a HiValue
countElems = M.map HiValueNumber . M.map fromInteger . M.fromListWith (+) . flip zip (repeat 1)

invert :: M.Map HiValue HiValue -> M.Map HiValue [HiValue]
invert m = M.fromListWith (++) pairs
    where pairs = [(v, [k]) | (k, v) <- M.toList m]
