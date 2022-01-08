{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Monad.Cont (lift, liftIO)
import Data.Set (fromList)
import HW3.Action
import HW3.Evaluator
import HW3.Parser
import HW3.Pretty
import Prettyprinter
import Prettyprinter.Render.Terminal (putDoc)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings loop
         where
           loop :: InputT IO ()
           loop = do
             minput <- getInputLine "hi> "
             case minput of
               Nothing -> return ()
               Just "q" -> return ()
               Just input -> do
                 case parse input of
                   Left e  -> lift $ putDoc $ pretty $ errorBundlePretty e
                   Right e -> do
                     res <- liftIO $ runHIO (eval e) (fromList [AllowTime, AllowRead, AllowWrite])
                     lift $ putDoc case res of
                       Left e'  -> prettyError e' <> line
                       Right e' -> prettyValue e' <> line
                 loop
