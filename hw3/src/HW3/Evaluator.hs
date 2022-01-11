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

-- | ExceptT is to big and dont't have a lot of information
type ExVal m = ExceptT HiError m HiValue

-- | evaluate expr when call evalE
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalE

{- | we have four type expr
  1) HiExprRun we need to run it
  2) HiExprValue we need to return it
  3) HiExprDict we need to create it
  4) HiExprApply we need evaluate it
-}
evalE :: HiMonad m => HiExpr -> ExVal m
evalE (HiExprRun exprun)          = runActExp exprun
evalE (HiExprValue val)           = return val
evalE (HiExprDict json)           = exprDict json
evalE (HiExprApply applyFun list) = evalE applyFun >>= flip getFun list

-- | runAction or throw HiErrorInvalidFunction
runActExp :: HiMonad m => HiExpr -> ExVal m
runActExp exprun = case exprun of
  HiExprValue (HiValueAction HiActionCwd) -> run HiActionCwd
  HiExprValue (HiValueAction HiActionNow) -> run HiActionNow
  hi@(HiExprApply _ _) -> evalE hi >>= \case
    HiValueAction action -> run action
    _ -> throwE HiErrorInvalidFunction
  _ -> throwE HiErrorInvalidFunction
  where
    run = lift . runAction

-- | create dict from list tupele expr
exprDict :: HiMonad m => [(HiExpr, HiExpr)] -> ExVal m
exprDict json = (mapM fun json) <&> (cons . M.fromList)
  where
    fun (a, b) = evalE a >>= (<&>) (evalE b) . (,)

-- | evaluate list exprs
evalList :: HiMonad m => [HiExpr] -> ExceptT HiError m [HiValue]
evalList list = mapM evalE list

-- | combinator for (pattern matching one argument) and (apply fun)
oneArg :: (Dep a, MaybeHi b, HiMonad m)
        => (b -> a) -> [HiExpr] -> ExVal m
oneArg fun = comBase one (cons . fun) . evalList

-- | combinator for (pattern matching two argument) and (apply uncurry fun)
twoArg :: (Dep a, MaybeHi b, MaybeHi c, HiMonad m)
        => (b -> c -> a) -> [HiExpr] -> ExVal m
twoArg fun = comBase two (cons . uncurry fun) . evalList

-- | combinator for (pattern matching some argument) and (apply Either fun)
com :: (HiMonad m) =>
 ([HiValue] -> Either HiError b1) -- parse
 -> (b1 -> Either HiError b2)     -- fun
 -> [HiExpr]                      -- evalList
 -> ExceptT HiError m b2
com parse fun = comBaseE parse (ExceptT . return . fun) . evalList

-- | combinator for (pattern matching some argument) and (apply fun)
comb :: HiMonad m =>
      ([HiValue] -> Either HiError a) -- parse
      -> (a -> HiValue)               -- fun
      -> [HiExpr]                     -- evalList
      -> ExVal m
comb parse fun = comBase parse fun . evalList

-- | return function from HiValue
getFun :: HiMonad m => HiValue -> ([HiExpr] -> ExVal m)
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
    HiFunSub            -> funTwoArgSub
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
    HiFunCount          -> count
    HiFunKeys           -> parseJSON fst
    HiFunValues         -> parseJSON snd
    HiFunInvert         -> invertJSON
  HiValueString str   -> indexFun str
  HiValueList   seq   -> indexFun seq
  HiValueBytes  byte  -> indexFun byte
  HiValueDict   json  -> comb one (fromMaybe (cons ()) . (`M.lookup` json))
  _                   -> \_ -> throwE HiErrorInvalidFunction

-- | lazy and
funAnd :: HiMonad m => [HiExpr] -> ExVal m
funAnd [a, b] = evalE a >>= \hi -> if (isTrue hi) then evalE b else return hi
funAnd _      = throwE HiErrorArityMismatch

-- | lazy or
funOr :: HiMonad m => [HiExpr] -> ExVal m
funOr [a, b]  = evalE a >>= \hi -> if (isTrue hi) then return hi else evalE b
funOr _       = throwE HiErrorArityMismatch

-- | lazy if, but normal if is lazy too
parseIf :: HiMonad m => [HiExpr] -> ExVal m
parseIf [evalE -> a, b, c] = a >>= \case
  (HiValueBool True)  -> evalE b
  (HiValueBool False) -> evalE c
  _                   -> throwE HiErrorInvalidArgument
parseIf _ = throwE HiErrorArityMismatch

-- | multiply two args
funTwoArgMul :: HiMonad m => [HiExpr] -> ExVal m
funTwoArgMul list = twoArg ((*) @Rational)                                 list
           <|> twoArg @Text          @Text          @Integer (flip stimes) list
           <|> twoArg @(Seq HiValue) @(Seq HiValue) @Integer (flip stimes) list
           <|> twoArg @ByteString    @ByteString    @Integer (flip stimes) list

-- | sum two args
funTwoArgAdd :: HiMonad m => [HiExpr] -> ExVal m
funTwoArgAdd list = twoArg ((+) @Rational) list
                <|> twoArg T.append        list
                <|> twoArg ((><) @HiValue) list
                <|> twoArg B.append        list
                <|> twoArg timeAdd         list
  where
    timeAdd = flip $ addUTCTime . secondsToNominalDiffTime . fromRational

-- | sub two args
funTwoArgSub :: HiMonad m => [HiExpr] -> ExVal m
funTwoArgSub list = twoArg ((-) @Rational)                list
                <|> twoArg ((toRational .) . diffUTCTime) list

-- | div two Rational or slesh two Text
funDivByZero :: HiMonad m => [HiExpr] -> ExVal m
funDivByZero list = twoArg slesh            list
                <|> com (two @Rational) div list
    where
      div (_, 0) = Left HiErrorDivideByZero
      div (a, b) = Right $ cons $ (/) a b

-- | get length from one arg
funTextLen :: HiMonad m => [HiExpr] -> ExVal m
funTextLen list = oneArg (toRational . len @Text)          list
              <|> oneArg (toRational . len @(Seq HiValue)) list
              <|> oneArg (toRational . len @(ByteString))  list

-- | get reverse from one arg
funTextRev :: HiMonad m => [HiExpr] -> ExVal m
funTextRev list = oneArg (T.reverse)          list
              <|> oneArg (S.reverse @HiValue) list
              <|> oneArg (B.reverse)          list

-- | fold some args, can use lazy function, but seq not lazy
foldFun :: HiMonad m => [HiExpr] -> ExVal m
foldFun = comBaseE two (uncurry foldFun') . evalList where
  foldFun' _   (Empty)           = return $ cons ()
  foldFun' _   (x :<| Empty)     = return x
  foldFun' val (F.toList -> seq) = fun `foldl1'` (fmap return seq)
    where
      fun a b = do
        a' <- a
        b' <- b
        getFun val [HiExprValue a', HiExprValue b']

-- | zip function use lazy bytes
zipFun :: HiMonad m => (L.ByteString -> L.ByteString) -> [HiExpr] -> ExVal m
zipFun fun = oneArg (L.toStrict . fun . L.fromStrict)

-- | we can write Text or ByteString to file
evalActionWrite :: HiMonad m => [HiExpr] -> ExVal m
evalActionWrite list = twoArg (flip funWrite) list
                   <|> twoArg funWrite'       list
    where
      funWrite' = HiActionWrite . T.unpack
      funWrite  = flip funWrite' . encodeUtf8

-- | count some args
count :: HiMonad m => [HiExpr] -> ExVal m
count list = oneArg (toDict . fmap toString . T.unpack) list
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

-- | common function for keys and value
parseJSON :: HiMonad m => ((HiValue, HiValue) -> HiValue) -> [HiExpr] -> ExVal m
parseJSON fun = oneArg (S.fromList . sort . fmap fun . M.toList)

-- | invert dict
invertJSON :: HiMonad m => [HiExpr] -> ExVal m
invertJSON = oneArg (M.fromListWith concat0 . pairs)
    where
      pairs :: Map HiValue HiValue -> [(HiValue, HiValue)]
      pairs m = [(v, cons $ S.fromList [k]) | (k, v) <- M.toList m]

      concat0 (HiValueList seq1) (HiValueList seq2) = cons (seq1 >< seq2)
      concat0 _ _ = error "this fine :)"

-- | index some type a
indexFun :: (Slice a, Index a, Dep a, HiMonad m)
           => a        -- колекция
           -> [HiExpr] -- аргументы
           -> ExVal m -- результат
indexFun lst list = comb one           (index lst)        list
                <|> comb (two @() @()) (\_ -> fun 0 endN) list
                <|> comb fstNull       (fun 0)            list
                <|> comb sndNull       (flip fun endN)    list
                <|> comb two           (uncurry fun)      list
  where
    fun a b = cons $ slice a b endN lst
    endN = len lst
