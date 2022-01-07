{-# LANGUAGE LambdaCase #-}

module HW3.Pretty where

import           HW3.Base

import           Data.ByteString               hiding (foldl1, foldr, map)
import           Data.Foldable                 (toList)
import           Data.Map                      (Map, assocs)
import           Data.Ratio
import           Data.Scientific               (FPFormat (..), formatScientific,
                                                fromRationalRepetendUnlimited)
import           Data.Sequence                 (Seq)
import qualified Data.Text                     as T (Text, pack)
import           Data.Time                     (UTCTime)
import           Data.Word
import           Numeric                       (showHex)
import           Prettyprinter                 hiding (prettyList)
import           Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = \case
  HiValueFunction fun  -> annotate (color Green) $ viaShow fun
  HiValueBool a        -> prettyBool a
  HiValueNumber a      -> prettyNumber a
  HiValueNull          -> prettyNull
  HiValueString text   -> prettyString text
  HiValueList li       -> prettyList li
  HiValueBytes bytes   -> prettyBytes bytes
  HiValueAction action -> prettyAction action
  HiValueTime time     -> prettyTime time
  HiValueDict assoc    -> prettyMap assoc

prettyMap :: Map HiValue HiValue -> Doc AnsiStyle
prettyMap assoc = braces $ space <>
  concatWith (\a b -> a <> comma <+> b) (map prettyPair $ assocs assoc)
  <> space where
  prettyPair :: (HiValue, HiValue) -> Doc AnsiStyle
  prettyPair (k, v) = prettyValue k <> pretty ":" <+> prettyValue v

prettyTime :: UTCTime -> Doc AnsiStyle
prettyTime time = pretty "parse-time" <> parens (prettyString $ T.pack $ show time)

prettyAction :: HiAction -> Doc AnsiStyle
prettyAction = \case
  HiActionCwd -> pretty "cwd"
  HiActionRead path -> pretty "read" <> parens (prettyString $ T.pack path)
  HiActionWrite path bytes -> pretty "read" <> parens
    (prettyString (T.pack path) <> comma <+> prettyBytes bytes)
  HiActionMkDir path -> pretty "mkdir" <> parens (prettyString $ T.pack path)
  HiActionChDir path -> pretty "cd" <> parens (prettyString $ T.pack path)
  HiActionNow -> pretty "now"
  HiActionRand i j -> pretty "rand" <> parens (space <> pretty i <> comma <+> pretty j <> space)
  HiActionEcho text -> pretty "echo" <> parens (prettyString text)

prettyBytes :: ByteString -> Doc AnsiStyle
prettyBytes b = pretty "[#" <+> pretty (unwords $ prettyHex <$> unpack b) <+> pretty "#]"

prettyHex :: Word8 -> String
prettyHex w | w < 16 = "0" ++ showHex w ""
            | otherwise = showHex w ""

prettyList :: Seq HiValue -> Doc AnsiStyle
prettyList = list . map prettyValue . toList

prettyString :: T.Text -> Doc AnsiStyle
prettyString text = annotate (color Blue) $ viaShow text

prettyNull :: Doc AnsiStyle
prettyNull = annotate (color Yellow) $ pretty "null"

prettyError :: HiError -> Doc AnsiStyle
prettyError = annotate (color Red) . viaShow

prettyBool :: Bool -> Doc AnsiStyle
prettyBool = \case
  True  -> pretty "true"
  False -> pretty "false"

prettyRational :: Integer -> Integer -> Doc AnsiStyle
prettyRational n d = pretty n <> pretty "/" <> pretty d

prettyNumber :: Rational -> Doc AnsiStyle
prettyNumber r =
  case fromRationalRepetendUnlimited r of
    (_, Just _) -> case quotRem (numerator r) (denominator r) of
       (n, 0) -> pretty n
       (0, d) -> prettyRational d $ denominator r
       (n, d) -> if d > 0
        then pretty n <+> pretty "+" <+> prettyRational d (denominator r)
        else pretty n <+> pretty "-" <+> prettyRational (-d) (denominator r)
    (res, _) -> case quotRem (numerator r) (denominator r) of
      (n, 0) -> pretty n
      _      -> pretty (formatScientific Fixed Nothing res)
