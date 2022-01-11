module HW3.Evaluator (eval) where

import Codec.Compression.Zlib
  (CompressParams(compressLevel), bestCompression, compressWith, decompressWith,
  defaultCompressParams, defaultDecompressParams)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Applicative (Alternative((<|>)))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import Data.Functor ((<&>))
import Data.List (foldl1', sort)
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
import HW3.Help
  (Dep(..), Index(..), MaybeHi(..), Slice(..), comBase, comBaseE, fstNull, isTrue, one, slesh,
  sndNull, two)
import Prelude hiding (div, map, seq)
import Text.Read (readMaybe)

type MyType m = ExceptT HiError m HiValue

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalE

evalE :: HiMonad m => HiExpr -> MyType m
evalE (HiExprRun exprun)          = runActExp exprun
evalE (HiExprValue val)           = return val
evalE (HiExprDict json)           = exprDict json
evalE (HiExprApply applyFun list) = evalE applyFun >>= \x -> getFun x list

runActExp :: HiMonad m => HiExpr -> MyType m
runActExp exprun = case exprun of
  HiExprValue (HiValueAction HiActionCwd) -> run HiActionCwd
  HiExprValue (HiValueAction HiActionNow) -> run HiActionNow
  hi@(HiExprApply _ _) -> evalE hi >>= \case
    HiValueAction action -> run action
    _ -> throwE HiErrorInvalidFunction
  _ -> throwE HiErrorInvalidFunction
  where
    run = lift . runAction

exprDict :: HiMonad m => [(HiExpr, HiExpr)] -> MyType m
exprDict json = (mapM fun json) <&> (cons . M.fromList)
  where
    fun (a, b) = evalE a >>= (<&>) (evalE b) . (,)

evalList :: HiMonad m => [HiExpr] -> ExceptT HiError m [HiValue]
evalList list = mapM evalE list

oneArg :: (Dep a, MaybeHi b, HiMonad m)
        => (b -> a) -> [HiExpr] -> MyType m
oneArg fun = comBase one (cons . fun) . evalList

twoArg :: (Dep a, MaybeHi b, MaybeHi c, HiMonad m)
        => (b -> c -> a) -> [HiExpr] -> MyType m
twoArg fun = comBase two (cons . uncurry fun) . evalList

com :: (HiMonad m) =>
 ([HiValue] -> Either HiError b1) -- parse
 -> (b1 -> Either HiError b2)     -- fun
 -> [HiExpr]                      -- evalList
 -> ExceptT HiError m b2
com parse fun = comBaseE parse (ExceptT . return . fun) . evalList

comb :: HiMonad m =>
      ([HiValue] -> Either HiError a) -- parse
      -> (a -> HiValue)               -- fun
      -> [HiExpr]                     -- evalList
      -> MyType m
comb parse fun = comBase parse fun . evalList

getFun :: HiMonad m => HiValue -> [HiExpr] -> MyType m
getFun = \case
  HiValueFunction fun -> case fun of
    HiFunNot            -> oneArg not
    HiFunAnd            -> funAnd
    HiFunOr             -> funOr
    HiFunLessThan       -> twoArg ((<)  @HiValue)
    HiFunGreaterThan    -> twoArg ((>)  @HiValue)
    HiFunEquals         -> twoArg ((==) @HiValue)
    HiFunNotLessThan    -> twoArg ((>=) @HiValue)
    HiFunNotGreaterThan -> twoArg ((<=) @HiValue)
    HiFunNotEquals      -> twoArg ((/=) @HiValue)
    HiFunIf             -> parseIf
    HiFunMul            -> funTwoArgMul
    HiFunAdd            -> funTwoArgAdd
    HiFunSub            -> funTwoArgMinus
    HiFunDiv            -> funDivByZero
    HiFunLength         -> funTextLen
    HiFunToUpper        -> oneArg T.toUpper
    HiFunToLower        -> oneArg T.toLower
    HiFunTrim           -> oneArg strip
    HiFunReverse        -> funTextRev
    HiFunList           -> comb return (cons . S.fromList)
    HiFunRange          ->
      twoArg (\a b -> S.fromList $ fmap (cons @Rational) [a ..b])
    HiFunFold           -> foldFun
    HiFunPackBytes      ->
      com (one @(Seq HiValue)) (fmap (cons . B.pack) . mapM maybehi . F.toList)
    HiFunUnpackBytes    ->
      oneArg (S.fromList . fmap (cons . toRational) . B.unpack)
    HiFunEncodeUtf8     -> oneArg (encodeUtf8)
    HiFunDecodeUtf8     -> comb one (either (\_ -> cons ()) cons . decodeUtf8')
    HiFunZip            -> zipFun
      (compressWith defaultCompressParams {compressLevel = bestCompression})
    HiFunUnzip          -> zipFun (decompressWith defaultDecompressParams)
    HiFunSerialise      -> oneArg (L.toStrict . serialise @HiValue)
    HiFunDeserialise    ->
      comb one (either (\_ -> cons ()) id . deserialiseOrFail . L.fromStrict)
    HiFunRead           -> oneArg (HiActionRead  . T.unpack)
    HiFunWrite          -> evalActionWrite
    HiFunMkDir          -> oneArg (HiActionMkDir . T.unpack)
    HiFunChDir          -> oneArg (HiActionChDir . T.unpack)
    HiFunParseTime      ->
      comb one (maybe (cons ()) (cons @UTCTime) . readMaybe . T.unpack)
    HiFunRand           -> twoArg HiActionRand
    HiFunEcho           -> oneArg HiActionEcho
    HiFunCount          -> countString
    HiFunKeys           -> parseJSON fst
    HiFunValues         -> parseJSON snd
    HiFunInvert         -> invertJSON
  HiValueString str   -> indexFun str
  HiValueList   seq   -> indexFun seq
  HiValueBytes  byte  -> indexFun byte
  HiValueDict   json  -> comb one (fromMaybe (cons ()) . (`M.lookup` json))
  _                   -> \_ -> throwE HiErrorInvalidFunction

funAnd :: HiMonad m => [HiExpr] -> MyType m
funAnd [a, b] = evalE a >>= \hi -> if (isTrue hi) then evalE b else return hi
funAnd _      = throwE HiErrorArityMismatch

funOr :: HiMonad m => [HiExpr] -> MyType m
funOr [a, b]  = evalE a >>= \hi -> if (isTrue hi) then return hi else evalE b
funOr _       = throwE HiErrorArityMismatch

parseIf :: HiMonad m => [HiExpr] -> MyType m
parseIf [evalE -> a, b, c] = a >>= \case
  (HiValueBool True)  -> evalE b
  (HiValueBool False) -> evalE c
  _                   -> throwE HiErrorInvalidArgument
parseIf _ = throwE HiErrorArityMismatch

funTwoArgMul :: HiMonad m => [HiExpr] -> MyType m
funTwoArgMul list = twoArg ((*) @Rational)                                 list
           <|> twoArg @Text          @Text          @Integer (flip stimes) list
           <|> twoArg @(Seq HiValue) @(Seq HiValue) @Integer (flip stimes) list
           <|> twoArg @ByteString    @ByteString    @Integer (flip stimes) list

funTwoArgAdd :: HiMonad m => [HiExpr] -> MyType m
funTwoArgAdd list = twoArg ((+) @Rational) list
                <|> twoArg T.append        list
                <|> twoArg ((><) @HiValue) list
                <|> twoArg B.append        list
                <|> twoArg timeAdd         list
  where
    timeAdd = flip $ addUTCTime . secondsToNominalDiffTime . fromRational

funTwoArgMinus :: HiMonad m => [HiExpr] -> MyType m
funTwoArgMinus list = twoArg ((-) @Rational)                list
                  <|> twoArg ((toRational .) . diffUTCTime) list

funDivByZero :: HiMonad m => [HiExpr] -> MyType m
funDivByZero list = twoArg slesh            list
                <|> com (two @Rational) div list
    where
      div (_, 0) = Left HiErrorDivideByZero
      div (a, b) = Right $ cons $ (/) a b

funTextLen :: HiMonad m => [HiExpr] -> MyType m
funTextLen list = oneArg (toRational . T.length)          list
              <|> oneArg (toRational . S.length @HiValue) list
              <|> oneArg (toRational . B.length)          list

funTextRev :: HiMonad m => [HiExpr] -> MyType m
funTextRev list = oneArg (T.reverse)          list
              <|> oneArg (S.reverse @HiValue) list
              <|> oneArg (B.reverse)          list

foldFun :: HiMonad m => [HiExpr] -> MyType m
foldFun = comBaseE two (uncurry foldFun') . evalList where
  foldFun' _   (Empty)           = return $ cons ()
  foldFun' _   (x :<| Empty)     = return x
  foldFun' val (F.toList -> seq) = fun `foldl1'` (fmap return seq)
    where
      fun a b = do 
        a' <- a
        b' <- b 
        getFun val [HiExprValue a', HiExprValue b']

zipFun :: HiMonad m => (L.ByteString -> L.ByteString) -> [HiExpr] -> MyType m
zipFun fun = oneArg (L.toStrict . fun . L.fromStrict)

evalActionWrite :: HiMonad m => [HiExpr] -> MyType m
evalActionWrite list = twoArg (flip funWrite) list
                   <|> twoArg funWrite'       list
    where
      funWrite' = HiActionWrite . T.unpack
      funWrite  = flip funWrite' . encodeUtf8

countString :: HiMonad m => [HiExpr] -> MyType m
countString list = oneArg (toDict . fmap toString . T.unpack) list
               <|> oneArg (toDict . F.toList @(Seq))          list
               <|> oneArg (toDict . fmap toByte . B.unpack)   list
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

parseJSON :: HiMonad m => ((HiValue, HiValue) -> HiValue) -> [HiExpr] -> MyType m
parseJSON fun = oneArg (S.fromList . sort . fmap fun . M.toList)

invertJSON :: HiMonad m => [HiExpr] -> MyType m
invertJSON = oneArg (M.fromListWith concat0 . pairs)
    where
      pairs :: Map HiValue HiValue -> [(HiValue, HiValue)]
      pairs m = [(v, cons $ S.fromList [k]) | (k, v) <- M.toList m]

      concat0 (HiValueList seq1) (HiValueList seq2) = cons (seq1 >< seq2)
      concat0 _ _ = error "this fine :)"

indexFun :: (Slice a, Index a, Dep a, HiMonad m)
           => a        -- колекция
           -> [HiExpr] -- аргументы
           -> MyType m -- результат
indexFun lst list = comb one           (index lst)        list
                <|> comb (two @() @()) (\_ -> fun 0 endN) list
                <|> comb fstNull       (fun 0)            list
                <|> comb sndNull       (flip fun endN)    list
                <|> comb two           (uncurry fun)      list
  where
    fun a b = cons $ slice a b endN lst
    endN = len lst
