{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HW3.Pretty
  ( prettyValue,
  )
where

import HW3.Base
import Prettyprinter (Doc, Pretty, pretty, viaShow, concatWith, surround)
import Prettyprinter.Render.Terminal (AnsiStyle)
import Data.Foldable (toList)
import Data.Ratio (numerator, denominator)
import Data.Scientific (fromRationalRepetendUnlimited, Scientific, toDecimalDigits)
import Data.List (intercalate)
import Data.ByteString (unpack)
import Numeric (showHex)

--log10 :: Integer -> Maybe Integer
--log10 x
--  | (x /= 1) && (x `mod` 10 /= 0) = Nothing
--  | x == 1 = Just 0
--  | otherwise = do
--    res <- log10 $ x `div` 10
--    return $ res + 1

prettyFraction :: Integer -> Integer -> Integer -> Integer -> Doc a
prettyFraction q r num den = do
  if q == 0 then
    pretty $ (show num) ++ "/" ++ (show den)
  else
    pretty $
      (if num < 0 then "-" else "")
      ++ (show (abs q))
      ++ (if num < 0 then " - " else " + ")
      ++ (show (abs r))
      ++ "/"
      ++ (show (abs den))

prettyFiniteDecimalFraction :: Scientific -> Doc a
prettyFiniteDecimalFraction scientific = do
  let (digits, offset) = toDecimalDigits (abs scientific)
  let prelude = if scientific < 0 then "-" else ""
  let digitsStr = (concatMap show digits)
  if offset >= 0 then
    if offset == 0 then
      pretty $ prelude ++ "0." ++ digitsStr
    else
      pretty $ prelude ++ (take offset digitsStr) ++ "." ++ (drop offset digitsStr)
  else
    pretty $ prelude ++ "0." ++ (replicate (abs offset) '0') ++ digitsStr

instance Pretty Rational where
  pretty :: Rational -> Doc a
  pretty x = do
    let num = numerator x
    let den = denominator x
    let (q, r) = quotRem num den
    if r == 0 then
      pretty q
    else
      let (scientific, repetend) = fromRationalRepetendUnlimited x in
        case repetend of
          Just _ -> prettyFraction q r num den
          Nothing -> prettyFiniteDecimalFraction scientific

instance Pretty HiFun where
  pretty :: HiFun -> Doc a
  pretty hiFun = case hiFun of
    HiFunDiv -> pretty "div"
    HiFunMul -> pretty "mul"
    HiFunAdd -> pretty "add"
    HiFunSub -> pretty "sub"
    HiFunNot -> pretty "not"
    HiFunAnd -> pretty "and"
    HiFunOr -> pretty "or"
    HiFunLessThan -> pretty "less-than"
    HiFunGreaterThan -> pretty "greater-than"
    HiFunEquals -> pretty "equals"
    HiFunNotLessThan -> pretty "not-less-than"
    HiFunNotGreaterThan -> pretty "not-greater-than"
    HiFunNotEquals -> pretty "not-equals"
    HiFunIf -> pretty "if"
    HiFunLength -> pretty "length"
    HiFunToUpper -> pretty "to-upper"
    HiFunToLower -> pretty "to-lower"
    HiFunReverse -> pretty "reverse"
    HiFunTrim -> pretty "trim"
    HiFunList -> pretty "list"
    HiFunRange -> pretty "range"
    HiFunFold -> pretty "fold"
    HiFunPackBytes -> pretty "pack-bytes"
    HiFunUnpackBytes -> pretty "unpack-bytes"
    HiFunEncodeUtf8 -> pretty "encode-utf8"
    HiFunDecodeUtf8 -> pretty "decode-utf8"
    HiFunZip -> pretty "zip"
    HiFunUnzip -> pretty "unzip"
    HiFunSerialise -> pretty "serialise"
    HiFunDeserialise -> pretty "deserialise"

instance Pretty HiValue where
  pretty :: HiValue -> Doc a
  pretty (HiValueFunction hiFun) = pretty hiFun
  pretty (HiValueNumber rational) = pretty rational
  pretty (HiValueBool bool) = pretty $ if bool then "true" else "false"
  pretty (HiValueNull) = pretty "null"
  pretty (HiValueString text) = viaShow text
  pretty (HiValueList values) = do
    let valuesList = toList values
    let prettyList = map pretty valuesList
    let inner = concatWith (surround (pretty ", ")) prettyList
    surround inner (pretty "[ ") (pretty (if null valuesList then "]" else " ]"))
  pretty (HiValueBytes bytes) = do
    let word8list = unpack bytes
    let hexStrList = map (\word8 -> (if word8 < 16 then "0" else "") ++ showHex word8 "") word8list
    let inner = concatWith (surround (pretty " ")) (map pretty hexStrList)
    surround inner (pretty "[# ") (pretty (if null word8list then "#]" else " #]"))


prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = pretty
