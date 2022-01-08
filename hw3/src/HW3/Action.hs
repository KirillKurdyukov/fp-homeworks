{-# LANGUAGE LambdaCase #-}

module HW3.Action where

import           Control.Exception
import           Control.Monad.Cont (ap, liftM)
import qualified Data.ByteString    as B (readFile, writeFile)
import qualified Data.Sequence      as S (fromList)
import           Data.Set           hiding (map)
import qualified Data.Text          as T
import           Data.Text.Encoding (decodeUtf8')
import           Data.Time          (getCurrentTime)
import           HW3.Base
import           System.Directory   (createDirectory, doesFileExist,
                                     getCurrentDirectory, listDirectory,
                                     setCurrentDirectory)
import           System.Random      (getStdRandom, uniformR)

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Eq, Ord, Show)

newtype PermissionException = PermissionRequired HiPermission deriving Show

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
  fmap = liftM

instance Applicative HIO where
  pure = return
  (<*>) = ap

instance Monad HIO where
  return a = HIO $ \_ -> return a
  m >>= k = HIO $ \env -> do
     v <- runHIO m env
     runHIO (k v) env

instance HiMonad HIO where
  runAction = \case
    HiActionCwd -> HIO $ \e ->
      if member AllowRead e
      then HiValueString . T.pack <$> getCurrentDirectory
      else throwIO $ PermissionRequired AllowRead
    HiActionRead path -> HIO $ \e ->
      if member AllowRead e then do
        isFile <- doesFileExist path
        if isFile
        then do
          bytes <- B.readFile path
          case decodeUtf8' bytes of
            Left _     -> return $ HiValueBytes bytes
            Right text -> return $ HiValueString text
        else (HiValueList . S.fromList) . fmap (HiValueString . T.pack) <$> listDirectory path
      else throwIO $ PermissionRequired AllowRead
    HiActionWrite path bytes -> HIO $ \e ->
      if member AllowWrite e then do
        B.writeFile path bytes
        return HiValueNull
      else throwIO $ PermissionRequired AllowWrite
    HiActionChDir path -> HIO $ \e ->
      if member AllowRead e then do
        setCurrentDirectory path
        return HiValueNull
      else throwIO $ PermissionRequired AllowRead
    HiActionMkDir path -> HIO $ \e ->
      if member AllowWrite e then do
        createDirectory path
        return HiValueNull
      else throwIO $ PermissionRequired AllowWrite
    HiActionNow -> HIO $ \e ->
      if member AllowTime e then HiValueTime <$> getCurrentTime
      else throwIO $ PermissionRequired AllowTime
    HiActionRand i j -> HIO $ \_ ->
     HiValueNumber . toRational <$> getStdRandom (uniformR (i, j))
    HiActionEcho text -> HIO $ \e ->
      if member AllowWrite e then do
        putStrLn (T.unpack text)
        return HiValueNull
      else throwIO $ PermissionRequired AllowWrite