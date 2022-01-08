module HW3.Evaluator (eval) where

import Codec.Compression.Zlib
  (CompressParams(compressLevel), bestCompression, compressWith, decompressWith,
  defaultCompressParams, defaultDecompressParams)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Applicative (Alternative((<|>)))
import Control.Monad ((>=>))
import Control.Monad.Trans.Except (ExceptT(..), throwE, runExceptT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(stimes))
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as S
import Data.Text (Text, strip)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, secondsToNominalDiffTime)
import HW3.Base (HiAction(..), HiError(..), HiExpr(..), HiFun(..), HiMonad(..), HiValue(..))
import HW3.Help
  (Dep(..), HiArgs(..), HiArgs2(..), IndexFun(..), MaybeHi(..), Sem(..), comBase, comBaseE, isTrue,
  slesh)
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
    HiFunNot            -> com one (cons . not)
    HiFunAnd            -> funAnd list
    HiFunOr             -> funOr list
    HiFunLessThan       -> funTwoArgEq (<)
    HiFunGreaterThan    -> funTwoArgEq (>)
    HiFunEquals         -> funTwoArgEq (==)
    HiFunNotLessThan    -> funTwoArgEq (>=)
    HiFunNotGreaterThan -> funTwoArgEq (<=)
    HiFunNotEquals      -> funTwoArgEq (/=)
    HiFunIf             -> parseIf list >>= evalE
    HiFunMul            -> funTwoArgMul
    HiFunAdd            -> funTwoArgAdd
    HiFunSub            -> funTwoArgMinus
    HiFunDiv            -> funDivByZero
    HiFunLength         -> funTextLen
    HiFunToUpper        -> com one (cons . T.toUpper)
    HiFunToLower        -> com one (cons . T.toLower)
    HiFunTrim           -> com one (cons . strip)
    HiFunReverse        -> funTextRev
    HiFunList           -> com return (cons . S.fromList)
    HiFunRange          ->
      com (two @Rational) $ \(a, b) -> cons . S.fromList $ fmap cons [a ..b]
    HiFunFold           -> comE oneone foldFun
    HiFunPackBytes      -> comE (one >=> return . (HiFunPackBytes,)) foldFun
    HiFunUnpackBytes    ->
      com one (cons . S.fromList . fmap (cons . toRational) . B.unpack)
    HiFunEncodeUtf8     -> com one (cons . encodeUtf8)
    HiFunDecodeUtf8     -> com one (either (\_ -> cons ()) cons . decodeUtf8')
    HiFunZip            -> zipFun
      (compressWith defaultCompressParams {compressLevel = bestCompression})
    HiFunUnzip          -> zipFun (decompressWith defaultDecompressParams)
    HiFunSerialise      -> com one (cons . L.toStrict . serialise @HiValue)
    HiFunDeserialis     ->
      com one (either (\_ -> cons ()) id . deserialiseOrFail . L.fromStrict)
    HiFunRead           -> com one (cons . HiActionRead  . T.unpack)
    HiFunWrite          -> evalActionWR
    HiFunMkDir          -> com one (cons . HiActionMkDir . T.unpack)
    HiFunChDir          -> com one (cons . HiActionChDir . T.unpack)
    HiFunParseTime      ->
      com one (maybe (cons ()) (cons @UTCTime) . readMaybe . T.unpack)
    HiFunRand           -> evalActionWR
    HiFunEcho           -> com one (cons . HiActionEcho)
    HiFunCount          -> countString
    HiFunKeys           -> parseJSON fst
    HiFunValues         -> parseJSON snd
    HiFunInvert         -> invertJSON
  HiValueString str   -> indexFun evalList str
  HiValueList seq     -> indexFun evalList seq
  HiValueBytes byte   -> indexFun evalList byte
  HiValueDict json    ->
    com (fmap cons . one @(Text)) $ fromMaybe (cons ()) . (`M.lookup` json)
  _                   -> throwE HiErrorInvalidFunction
 where
  evalList :: ExceptT HiError m [HiValue]
  evalList = mapM evalE list

  com :: ([HiValue] -> Either HiError a)
      -> (a -> HiValue)
      -> ExceptT HiError m HiValue
  com = comBase evalList

  comE :: ([HiValue] -> Either HiError a)
       -> (a -> Either HiError HiValue)
       -> ExceptT HiError m HiValue
  comE = comBaseE evalList

  funDivByZero :: ExceptT HiError m HiValue
  funDivByZero = com  two (cons . uncurry slesh)
             <|> comE (two @Rational) div
    where
      div (_, 0) = Left HiErrorDivideByZero
      div (a, b) = Right $ cons $ (/) a b

  funAnd :: [HiExpr] -> ExceptT HiError m HiValue
  funAnd [a, b] = evalE a >>= \hi -> if (isTrue hi) then evalE b else return hi
  funAnd _ = throwE HiErrorArityMismatch

  funOr :: [HiExpr] -> ExceptT HiError m HiValue
  funOr [a, b] = evalE a >>= \hi -> if (isTrue hi) then return hi else evalE b
  funOr _ = throwE HiErrorArityMismatch

  funTwoArgMinus :: ExceptT HiError m HiValue
  funTwoArgMinus = com two (cons . uncurry ((-) @Rational))
               <|> com two (cons . uncurry fun)
    where
      fun t1 t2 = (toRational $ diffUTCTime t1 t2)

  funTwoArgAdd :: ExceptT HiError m HiValue
  funTwoArgAdd = com two    (cons . uncurry ((+) @Rational))
             <|> com two    (cons . uncurry T.append)
             <|> com two    (cons . uncurry ((><) @HiValue))
             <|> com two    (cons . uncurry B.append)
             <|> com oneone (cons . uncurry fun)
    where
      fun time (fromRational -> num) =
        addUTCTime (secondsToNominalDiffTime (num)) time

  funTwoArgMul :: ExceptT HiError m HiValue
  funTwoArgMul = com two (cons . uncurry ((*) @Rational))
             <|> com (oneone @(Sem Text)          @Integer) fun
             <|> com (oneone @(Sem (Seq HiValue)) @Integer) fun
             <|> com (oneone @(Sem ByteString)    @Integer) fun
    where
      fun ((Sem s), int) = cons $ (stimes int s)

  funTwoArgEq :: (HiValue -> HiValue -> Bool) -> ExceptT HiError m HiValue
  funTwoArgEq fun = com return (\[a, b] -> cons $ fun a b)

  funTextLen :: ExceptT HiError m HiValue
  funTextLen = com one (toNum . T.length)
           <|> com one (toNum . S.length @HiValue)
    where toNum = cons . toRational

  funTextRev :: ExceptT HiError m HiValue
  funTextRev = com one (cons . T.reverse)
           <|> com one (cons . S.reverse @HiValue)

  foldFun :: (HiFun, Seq HiValue) -> Either HiError HiValue
  foldFun (_, S.Empty) = return $ cons ()
  foldFun (fun, seq)   = case fun of
    HiFunDiv       -> foldSeq (/)
    HiFunMul       -> foldSeq (*)
    HiFunAdd       -> foldSeq (+)
    HiFunSub       -> foldSeq (-)
    HiFunFold      -> (oneone $ F.toList seq) >>= foldFun
    HiFunPackBytes -> (mapM maybehi . F.toList $ seq) <&> cons . B.pack
    _              -> Left HiErrorInvalidArgument
    where
      foldSeq :: (Rational -> Rational -> Rational) -> Either HiError HiValue
      foldSeq f = (mapM maybehi . F.toList) seq <&> cons . (f `F.foldl1`)

  zipFun :: (L.ByteString -> L.ByteString) -> ExceptT HiError m HiValue
  zipFun fun = com one (cons . L.toStrict . fun . L.fromStrict)

  evalActionWR :: ExceptT HiError m HiValue
  evalActionWR = com two (cons . funWrite)
             <|> com oneone (cons . funWrite')
             <|> com (two @Int) (cons . uncurry HiActionRand)
    where
      funWrite' (T.unpack -> t1, byte) = HiActionWrite t1 byte
      funWrite (text, encodeUtf8 -> t2) = funWrite' (text, t2)

  parseJSON :: ((HiValue, HiValue) -> HiValue) -> ExceptT HiError m HiValue
  parseJSON fun = com one (cons . S.fromList . sort . fmap fun . M.toList)

  countString :: ExceptT HiError m HiValue
  countString = com one (toDict . fmap toString . T.unpack)
            <|> com one (toDict . F.toList @(Seq))
            <|> com one (toDict . fmap toByte . B.unpack)
    where
      toDict = cons . M.fromList . listZip
      toByte = cons . (toEnum @Rational) . fromEnum
      toString a = cons $ T.pack [a]

      listZip :: [HiValue] -> [(HiValue, HiValue)]
      listZip seq = zipWith
        (\ a b -> (a, cons $ (toEnum @Rational) b))
        seq
        (fmap (countVal seq) seq)

      countVal str = length . flip filter str . (==)

  invertJSON :: ExceptT HiError m HiValue
  invertJSON = com one (cons . M.fromListWith concat0 . pairs)
    where
      pairs :: Map HiValue HiValue -> [(HiValue, HiValue)]
      pairs m = [(v, cons $ S.fromList [k]) | (k, v) <- M.toList m]

      concat0 (HiValueList seq1) (HiValueList seq2) = cons (seq1 >< seq2)
      concat0 _ _ = error "this fine :)"
