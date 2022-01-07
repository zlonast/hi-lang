module HW3.Base where

import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Prelude hiding (map, seq)

data HiFun =
  HiFunDiv
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
  | HiFunDeserialis
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
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialise)

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialise)

data HiValue =
  HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialise)

data HiExpr =
  HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialise)

data HiError =
  HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialise)

instance Semigroup HiError where
  (<>) a HiErrorArityMismatch   = a
  (<>) HiErrorArityMismatch a   = a
  (<>) _ HiErrorInvalidFunction = HiErrorInvalidFunction
  (<>) HiErrorInvalidFunction _ = HiErrorInvalidFunction
  (<>) _ HiErrorDivideByZero    = HiErrorDivideByZero
  (<>) HiErrorDivideByZero _    = HiErrorDivideByZero
  (<>) HiErrorInvalidArgument HiErrorInvalidArgument = HiErrorInvalidArgument

instance Monoid HiError where
  mempty = HiErrorArityMismatch

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
