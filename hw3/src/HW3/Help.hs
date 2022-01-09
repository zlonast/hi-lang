module HW3.Help (
  Dep(..),
  one,
  two, 
  MaybeHi(..),
  Index(..),
  Slice(..),
  IndexFun(..),
  slesh,
  comBase,
  comBaseE,
  isTrue) where

import Control.Applicative (Alternative((<|>)))
import Control.Lens (Getting, makePrisms, (^?))
import Control.Monad ((>=>))
import Control.Monad.Trans.Except (ExceptT(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Map (Map)
import Data.Monoid (First)
import Data.Ratio (denominator, numerator)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Word (Word8)
import HW3.Base (HiAction, HiError(..), HiFun, HiValue(..))
import Prelude hiding (drop, seq, take)

$(makePrisms ''HiValue)

class Dep a where
  cons :: a -> HiValue

instance Dep Bool                  where cons = HiValueBool
instance Dep Rational              where cons = HiValueNumber
instance Dep HiFun                 where cons = HiValueFunction
instance Dep ()                    where cons = \_ -> HiValueNull
instance Dep Text                  where cons = HiValueString
instance Dep (Seq HiValue)         where cons = HiValueList
instance Dep ByteString            where cons = HiValueBytes
instance Dep HiAction              where cons = HiValueAction
instance Dep UTCTime               where cons = HiValueTime
instance Dep (Map HiValue HiValue) where cons = HiValueDict

tranc :: Rational -> Either HiError Integer
tranc num = case denominator num == 1 of 
  True  -> Right $ numerator num
  False -> Left HiErrorInvalidArgument

word8 :: Int -> Either HiError Word8
word8 n | n < 0 || n > 255 = Left HiErrorInvalidArgument
        | otherwise = Right . toEnum $ n

toInt :: Integer -> Either HiError Int
toInt n | n <= (toInteger (maxBound :: Int))
       && n >= (toInteger (minBound :: Int)) = Right $ fromEnum n
        | otherwise = Left HiErrorInvalidArgument

one :: MaybeHi a => [HiValue] -> Either HiError a
one [maybehi -> a] = a
one _ = Left HiErrorArityMismatch

two :: (MaybeHi a, MaybeHi b) => [HiValue] -> Either HiError (a, b)
two [maybehi -> a, maybehi -> b] = a >>= flip fmap b . (,)
two _ = Left HiErrorArityMismatch

class MaybeHi a where
  maybehi :: HiValue -> Either HiError a

except :: Getting (First b) s b -> s -> Either HiError b
except prism foo = case foo ^? prism of
  Nothing -> Left HiErrorInvalidArgument
  Just a  -> Right a

instance MaybeHi Integer               where maybehi = (=<<) tranc . maybehi
instance MaybeHi Int                   where maybehi = (=<<) toInt . maybehi @Integer
instance MaybeHi Word8                 where maybehi = (=<<) word8 . maybehi
instance MaybeHi Bool                  where maybehi = except _HiValueBool
instance MaybeHi Rational              where maybehi = except _HiValueNumber
instance MaybeHi HiFun                 where maybehi = except _HiValueFunction
instance MaybeHi Text                  where maybehi = except _HiValueString
instance MaybeHi (Seq HiValue)         where maybehi = except _HiValueList
instance MaybeHi ByteString            where maybehi = except _HiValueBytes
instance MaybeHi HiAction              where maybehi = except _HiValueAction
instance MaybeHi UTCTime               where maybehi = except _HiValueTime
instance MaybeHi (Map HiValue HiValue) where maybehi = except _HiValueDict
instance MaybeHi HiValue               where maybehi = return
instance MaybeHi ()                    where maybehi = except _HiValueNull

slesh :: Text -> Text -> Text
slesh a = T.append (T.append a (T.pack ['/']))

class Index a where
  index :: a -> Int -> HiValue
  index a n | n < 0 || n > len a = cons ()
            | otherwise          = find a n
  len  :: a -> Int
  find :: a -> Int -> HiValue

instance Index Text where
  len = T.length
  find text n = cons $ T.pack [T.index text n]

instance Index (Seq HiValue) where
  len = S.length
  find seq = S.index seq

instance Index ByteString where
  len = B.length
  find byte = cons . toRational . B.index byte

class Slice a where
  slice :: Int -> Int -> Int -> a -> a
  slice st en n | st < 0 && en < 0 = slice (st + n) (en + n) n
                | st < 0           = slice (st + n) en n
                | en < 0           = slice st (en + n) n
                | otherwise        = take (en - st) . drop st
  drop :: Int -> a -> a
  take :: Int -> a -> a

instance Slice Text where
  drop = T.drop
  take = T.take

instance Slice (Seq HiValue) where
  drop = S.drop
  take = S.take

instance Slice ByteString where
  drop = B.drop
  take = B.take

class (Slice a, Index a, Dep a) => IndexFun a where
  indexFun :: Monad m
           => ExceptT HiError m [HiValue]
           -> a
           -> ExceptT HiError m HiValue
  indexFun list lst = comBase list one           (index lst)
                  <|> comBase list (two @() @()) (\_ -> fun 0 endN)
                  <|> comBase list sndNum        (fun 0)
                  <|> comBase list fstNum        (flip fun endN)
                  <|> comBase list two           (uncurry fun)
    where
      fun a b = cons $ (slice a b (len lst)) lst
      endN = (len lst)

instance IndexFun Text
instance IndexFun (Seq HiValue)
instance IndexFun ByteString

sndNum :: [HiValue] -> Either HiError Int
sndNum [HiValueNull, maybehi -> num] = num
sndNum [_,_] = Left HiErrorInvalidArgument
sndNum _ = Left HiErrorArityMismatch

fstNum :: [HiValue] -> Either HiError Int
fstNum [maybehi -> num, HiValueNull] = num
fstNum [_,_] = Left HiErrorInvalidArgument
fstNum _ = Left HiErrorArityMismatch

comBase :: Monad m
        => ExceptT e m a1
        -> (a1 -> Either e a2)
        -> (a2 -> b)
        -> ExceptT e m b
comBase evalList parse fun = comBaseE evalList parse (return . fun)

comBaseE :: Monad m
         => ExceptT e m a
         -> (a -> Either e b1)
         -> (b1 -> Either e b2)
         -> ExceptT e m b2
comBaseE evalList parse fun = evalList >>= (ExceptT . return . (parse >=> fun))

isTrue :: HiValue -> Bool
isTrue HiValueNull         = False
isTrue (HiValueBool False) = False
isTrue _                   = True
