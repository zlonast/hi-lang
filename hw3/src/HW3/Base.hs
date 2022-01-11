module HW3.Base (
  HiFun(..),
  HiAction(..),
  HiValue(..),
  HiExpr(..),
  HiError(..),
  HiMonad(..)) where

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
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Serialise)

instance Show HiFun where
  show = \case
    HiFunNot            -> "not"
    HiFunDiv            -> "div"
    HiFunMul            -> "mul"
    HiFunAdd            -> "add"
    HiFunAnd            -> "and"
    HiFunOr             -> "or"
    HiFunLessThan       -> "less-than"
    HiFunGreaterThan    -> "greater-than"
    HiFunEquals         -> "equals"
    HiFunNotLessThan    -> "not-less-than"
    HiFunNotGreaterThan -> "not-greater-than"
    HiFunNotEquals      -> "not-equals"
    HiFunIf             -> "if"
    HiFunSub            -> "sub"
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

{- | Algebra of logic for consecutive parsers
  for example oneArg funUseSeq
          <|> oneArg funUseByte
  if passed to Apply Bytes ignore the error of the first parser
  but the situation is not always so simple

  for example oneArg funIndex
        <|> twoArg funSlice
  if we passed one argument, then we execute the first function, if passed two,
  then the second, if a different number was passed, then HiErrorArityMismatch
-}
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
