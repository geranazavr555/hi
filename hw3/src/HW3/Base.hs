{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module HW3.Base
  ( HiFun(..),
    HiValue(..),
    HiExpr(..),
    HiError(..)
  )
where

import Data.Text (Text)
import Data.Sequence (Seq)
import Data.ByteString (ByteString)
import Codec.Serialise (Serialise)
import GHC.Generics (Generic)

-- | Function names (e.g. div, sort, length, ...)
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
  | HiFunIf
  deriving (Show, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise)

-- | Values (numbers, booleans, strings, ...)
data HiValue
  = HiValueBool Bool
  | HiValueNumber Rational
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueFunction HiFun
  deriving (Show, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise)

-- | Expressions (literals, function calls, ...)
data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  deriving (Show, Eq)
  deriving stock (Generic)
  deriving anyclass (Serialise)

-- | Evaluation errors (invalid arguments, ...)
data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq)
  deriving stock (Generic)
  deriving anyclass (Serialise)
