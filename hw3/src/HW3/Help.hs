module HW3.Help (
  Dep(..),
  one,
  two,
  MaybeHi(..),
  Index(..),
  Slice(..),
  slesh,
  comBase,
  comBaseE,
  fstNull,
  sndNull,
  isTrue) where

import Control.Lens (Getting, makePrisms, (^?))
import Control.Monad ((>=>))
import Control.Monad.Trans (lift)
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

{- | conceptually, this whole module is auxiliary functions
for example, I wanted to make a common constructor of all functions
for example, I wanted to make general patermachin
for example typeclasses
-}

-- | make prisms, since the lebe are unsafe
$(makePrisms ''HiValue)

-- | we need common constructor to delete boilerplate
class Dep a where
  cons :: a -> HiValue

-- | maybe we need use TH
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
instance Dep HiValue               where cons = id

-- | we want fall when have float
tranc :: Rational -> Either HiError Integer
tranc num = case denominator num == 1 of
  True  -> Right $ numerator num
  False -> Left HiErrorInvalidArgument

-- | if we have bigger or less then we want fall
word8 :: Int -> Either HiError Word8
word8 n | n < 0 || n > 255 = Left HiErrorInvalidArgument
        | otherwise = Right . toEnum $ n

-- | it to big for int then fall
toInt :: Integer -> Either HiError Int
toInt n | n <= (toInteger (maxBound :: Int))
       && n >= (toInteger (minBound :: Int)) = Right $ fromEnum n
        | otherwise = Left HiErrorInvalidArgument

-- | pattern matching one argument
one :: MaybeHi a => [HiValue] -> Either HiError a
one [maybehi -> a] = a
one _ = Left HiErrorArityMismatch

-- | pattern matching two argument
two :: (MaybeHi a, MaybeHi b) => [HiValue] -> Either HiError (a, b)
two [maybehi -> a, maybehi -> b] = a >>= flip fmap b . (,)
two _ = Left HiErrorArityMismatch

-- | maybe parse HiValue in value
class MaybeHi a where
  maybehi :: HiValue -> Either HiError a

-- | i try to use lens :)
except :: Getting (First b) s b -> s -> Either HiError b
except prism foo = case foo ^? prism of
  Nothing -> Left HiErrorInvalidArgument
  Just a  -> Right a

-- | maybe we want TH there
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

-- | function use text ++ / ++ text
slesh :: Text -> Text -> Text
slesh a = T.append (T.append a (T.pack ['/']))

-- | typeclass for create common indexing some types
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

-- | common function slice for some types
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

-- | less base combinator then comBaseE
comBase :: Monad m
        => (a1 -> Either e a2)  -- parse
        -> (a2 -> b)            -- fun
        -> ExceptT e m a1       -- evalList
        -> ExceptT e m b
comBase parse fun = comBaseE parse (lift . return . fun)

-- | very base combinator
comBaseE :: Monad m
         => (a -> Either e b1)     -- parse
         -> (b1 -> ExceptT e m b2) -- fun
         -> ExceptT e m a          -- evalList
         -> ExceptT e m b2
comBaseE parse fun evalList = evalList >>= (ExceptT . return . parse >=> fun)

-- | parse two args, but first is null
fstNull :: MaybeHi a => [HiValue] -> Either HiError a
fstNull [HiValueNull, maybehi -> num] = num
fstNull [_,_] = Left HiErrorInvalidArgument
fstNull _ = Left HiErrorArityMismatch

-- | parse two args, but second is null
sndNull :: MaybeHi a => [HiValue] -> Either HiError a
sndNull [maybehi -> num, HiValueNull] = num
sndNull [_,_] = Left HiErrorInvalidArgument
sndNull _ = Left HiErrorArityMismatch

-- | null is same as false
isTrue :: HiValue -> Bool
isTrue HiValueNull         = False
isTrue (HiValueBool False) = False
isTrue _                   = True
