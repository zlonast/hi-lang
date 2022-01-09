module HW3.Evaluator (eval) where

import Codec.Compression.Zlib
  (CompressParams(compressLevel), bestCompression, compressWith, decompressWith,
  defaultCompressParams, defaultDecompressParams)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Applicative (Alternative((<|>)))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import Data.Functor ((<&>))
import Data.List (sort, foldl1')
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(stimes))
import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as S
import Data.Text (Text, strip)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, secondsToNominalDiffTime)
import HW3.Base (HiAction(..), HiError(..), HiExpr(..), HiFun(..), HiMonad(..), HiValue(..))
import HW3.Help (Dep(..), IndexFun(..), MaybeHi(..), comBase, comBaseE, isTrue, one, slesh, two)
import Prelude hiding (div, map, seq)
import Text.Read (readMaybe)

parseIf :: HiMonad m => [HiExpr] -> ExceptT HiError m HiExpr
parseIf [evalE -> a, b, c] = a >>= \case
  (HiValueBool True)  -> return b
  (HiValueBool False) -> return c
  _                   -> throwE HiErrorInvalidArgument
parseIf _ = throwE HiErrorArityMismatch

exprDict :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptT HiError m HiValue
exprDict json = (mapM fun json) <&> (cons . M.fromList)
  where
    fun (a, b) = evalE a >>= (<&>) (evalE b) . (,)

runActExp :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
runActExp exprun = case exprun of
  HiExprValue (HiValueAction HiActionCwd) -> run HiActionCwd
  HiExprValue (HiValueAction HiActionNow) -> run HiActionNow
  hi@(HiExprApply _ _) -> evalE hi >>= \case
    HiValueAction action -> run action
    _ -> throwE HiErrorInvalidFunction
  _ -> throwE HiErrorInvalidFunction
  where
    run = ExceptT . fmap Right . runAction

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalE

evalE :: forall m . HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalE (HiExprRun exprun)          = runActExp exprun
evalE (HiExprValue val)           = return val
evalE (HiExprDict json)           = exprDict json
evalE (HiExprApply applyFun list) = evalE applyFun >>= \case
  HiValueFunction fun -> case fun of
    HiFunNot            -> oneArg not
    HiFunAnd            -> funAnd list
    HiFunOr             -> funOr list
    HiFunLessThan       -> twoArg ((<)  @HiValue)
    HiFunGreaterThan    -> twoArg ((>)  @HiValue)
    HiFunEquals         -> twoArg ((==) @HiValue)
    HiFunNotLessThan    -> twoArg ((>=) @HiValue)
    HiFunNotGreaterThan -> twoArg ((<=) @HiValue)
    HiFunNotEquals      -> twoArg ((/=) @HiValue)
    HiFunIf             -> parseIf list >>= evalE
    HiFunMul            -> funTwoArgMul
    HiFunAdd            -> funTwoArgAdd
    HiFunSub            -> funTwoArgMinus
    HiFunDiv            -> funDivByZero
    HiFunLength         -> funTextLen
    HiFunToUpper        -> oneArg T.toUpper
    HiFunToLower        -> oneArg T.toLower
    HiFunTrim           -> oneArg strip
    HiFunReverse        -> funTextRev
    HiFunList           -> return -|>|> (cons . S.fromList)
    HiFunRange          ->
      twoArg (\a b -> S.fromList $ fmap (cons @Rational) [a ..b])
    HiFunFold           -> two -|>|>* foldFun
    HiFunPackBytes      ->
      one @(Seq HiValue) -|>|>* (fmap (cons . B.pack) . mapM maybehi . F.toList)
    HiFunUnpackBytes    ->
      oneArg (S.fromList . fmap (cons . toRational) . B.unpack)
    HiFunEncodeUtf8     -> oneArg (encodeUtf8)
    HiFunDecodeUtf8     -> one -|>|> (either (\_ -> cons ()) cons . decodeUtf8')
    HiFunZip            -> zipFun
      (compressWith defaultCompressParams {compressLevel = bestCompression})
    HiFunUnzip          -> zipFun (decompressWith defaultDecompressParams)
    HiFunSerialise      -> oneArg (L.toStrict . serialise @HiValue)
    HiFunDeserialis     ->
      one -|>|> (either (\_ -> cons ()) id . deserialiseOrFail . L.fromStrict)
    HiFunRead           -> oneArg (HiActionRead  . T.unpack)
    HiFunWrite          -> evalActionWrite
    HiFunMkDir          -> oneArg (HiActionMkDir . T.unpack)
    HiFunChDir          -> oneArg (HiActionChDir . T.unpack)
    HiFunParseTime      ->
      one -|>|> (maybe (cons ()) (cons @UTCTime) . readMaybe . T.unpack)
    HiFunRand           -> twoArg HiActionRand
    HiFunEcho           -> oneArg HiActionEcho
    HiFunCount          -> countString
    HiFunKeys           -> parseJSON fst
    HiFunValues         -> parseJSON snd
    HiFunInvert         -> invertJSON
  HiValueString str   -> indexFun evalList str
  HiValueList   seq   -> indexFun evalList seq
  HiValueBytes  byte  -> indexFun evalList byte
  HiValueDict   json  ->
    (fmap cons . one @(Text)) -|>|> (fromMaybe (cons ()) . (`M.lookup` json))
  _                   -> throwE HiErrorInvalidFunction
 where
  evalList :: ExceptT HiError m [HiValue]
  evalList = mapM evalE list

  (-|>|>) :: ([HiValue] -> Either HiError a)
      -> (a -> HiValue)
      -> ExceptT HiError m HiValue
  (-|>|>) = comBase evalList

  (-|>|>*) :: ([HiValue] -> Either HiError a)
       -> (a -> Either HiError HiValue)
       -> ExceptT HiError m HiValue
  (-|>|>*) = comBaseE evalList

  oneArg :: (Dep a, MaybeHi b) => (b -> a) -> ExceptT HiError m HiValue
  oneArg fun = comBase evalList one (cons . fun)

  twoArg :: (Dep a, MaybeHi b, MaybeHi c) => (b -> c -> a) -> ExceptT HiError m HiValue
  twoArg fun = comBase evalList two (cons . uncurry fun)

  funDivByZero :: ExceptT HiError m HiValue
  funDivByZero = twoArg slesh
             <|> (two @Rational) -|>|>* div
    where
      div (_, 0) = Left HiErrorDivideByZero
      div (a, b) = Right $ cons $ (/) a b

  funAnd :: [HiExpr] -> ExceptT HiError m HiValue
  funAnd [a, b] = evalE a >>= \hi -> if (isTrue hi) then evalE b else return hi
  funAnd _      = throwE HiErrorArityMismatch

  funOr :: [HiExpr] -> ExceptT HiError m HiValue
  funOr [a, b]  = evalE a >>= \hi -> if (isTrue hi) then return hi else evalE b
  funOr _       = throwE HiErrorArityMismatch

  funTwoArgMinus :: ExceptT HiError m HiValue
  funTwoArgMinus = twoArg ((-) @Rational)
               <|> twoArg ((toRational .) . diffUTCTime)

  funTwoArgAdd :: ExceptT HiError m HiValue
  funTwoArgAdd = twoArg ((+) @Rational)
             <|> twoArg T.append
             <|> twoArg ((><) @HiValue)
             <|> twoArg B.append
             <|> twoArg timeAdd
    where
      timeAdd = flip $ addUTCTime . secondsToNominalDiffTime . fromRational

  funTwoArgMul :: ExceptT HiError m HiValue
  funTwoArgMul = twoArg ((*) @Rational)
             <|> twoArg @Text          @Text          @Integer (flip stimes)
             <|> twoArg @(Seq HiValue) @(Seq HiValue) @Integer (flip stimes)
             <|> twoArg @ByteString    @ByteString    @Integer (flip stimes)

  funTextLen :: ExceptT HiError m HiValue
  funTextLen = oneArg (toRational . T.length)
           <|> oneArg (toRational . S.length @HiValue)

  funTextRev :: ExceptT HiError m HiValue
  funTextRev = oneArg (T.reverse)
           <|> oneArg (S.reverse @HiValue)

  foldFun :: (HiFun, Seq HiValue) -> Either HiError HiValue
  foldFun (_, Empty) = return $ cons ()
  foldFun (fun, seq) = case fun of
    HiFunDiv       -> foldSeq @Rational (/)
    HiFunMul       -> foldSeq @Rational (*)
    HiFunAdd       -> foldSeq @Rational (+)
    HiFunSub       -> foldSeq @Rational (-)
    HiFunFold      -> (two $ F.toList seq) >>= foldFun
    _              -> Left HiErrorInvalidArgument
    where
      foldSeq :: (MaybeHi a, Dep a) => (a -> a -> a) -> Either HiError HiValue
      foldSeq f = (mapM maybehi . F.toList) seq <&> cons . (f `foldl1'`)

  zipFun :: (L.ByteString -> L.ByteString) -> ExceptT HiError m HiValue
  zipFun fun = oneArg (L.toStrict . fun . L.fromStrict)

  evalActionWrite :: ExceptT HiError m HiValue
  evalActionWrite = twoArg (flip funWrite)
                <|> twoArg funWrite'
    where
      funWrite' = HiActionWrite . T.unpack
      funWrite  = flip funWrite' . encodeUtf8

  parseJSON :: ((HiValue, HiValue) -> HiValue) -> ExceptT HiError m HiValue
  parseJSON fun = oneArg (S.fromList . sort . fmap fun . M.toList)

  countString :: ExceptT HiError m HiValue
  countString = oneArg (toDict . fmap toString . T.unpack)
            <|> oneArg (toDict . F.toList @(Seq))
            <|> oneArg (toDict . fmap toByte . B.unpack)
    where
      toDict = M.fromList . listZip
      toByte = cons . (toEnum @Rational) . fromEnum
      toString a = cons $ T.pack [a]

      listZip :: [HiValue] -> [(HiValue, HiValue)]
      listZip seq = zipWith
        (\ a b -> (a, cons $ (toEnum @Rational) b))
        seq
        (fmap (countVal seq) seq)

      countVal str = length . flip filter str . (==)

  invertJSON :: ExceptT HiError m HiValue
  invertJSON = oneArg (M.fromListWith concat0 . pairs)
    where
      pairs :: Map HiValue HiValue -> [(HiValue, HiValue)]
      pairs m = [(v, cons $ S.fromList [k]) | (k, v) <- M.toList m]

      concat0 (HiValueList seq1) (HiValueList seq2) = cons (seq1 >< seq2)
      concat0 _ _ = error "this fine :)"
