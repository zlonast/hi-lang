module HW3.Pretty (prettyValue) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (Foldable(toList))
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Ratio (denominator, numerator)
import Data.Scientific (FPFormat(..), formatScientific, fromRationalRepetendUnlimited)
import Data.Tuple (swap)
import HW3.Base (HiAction(..), HiValue(..))
import Numeric (showHex)
import Prettyprinter (Doc, Pretty(pretty), list, viaShow)
import Prettyprinter.Render.Terminal (AnsiStyle)

-- | print hivalue
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue hi = case hi of
  HiValueNumber num   -> pretty $ showRational num
  HiValueFunction fun -> viaShow fun
  HiValueBool bool    -> pretty $ if bool then "true" else "false"
  HiValueNull         -> pretty "null"
  HiValueString str   -> viaShow str
  HiValueList lst     -> list $ map prettyValue (toList lst)
  HiValueBytes bytes  -> pretty $ showByteString bytes
  HiValueAction act   -> prettyAction act
  HiValueTime time    -> pretty $ "parse-time(" ++ show (show time) ++ ")"
  HiValueDict dict    -> pretty $ "{ " ++ showDict (M.toList dict) ++ " }"

-- | parse numbers
showRational :: Rational -> String
showRational num =
  if denominator num == 1 then
    show (numerator num)
  else case (fromRationalRepetendUnlimited num) of
    tup@(_, Nothing) -> uncurry (formatScientific Fixed) $ swap $ tup
    (_, Just _) -> case quotRem (numerator num) (denominator num) of
      (n, i) -> case n of
        0 -> show i ++ "/" ++ show (denominator num)
        _ -> if i < 0 then
            show n ++ " - " ++ show (i * (-1)) ++ "/" ++ show (denominator num)
          else
            show n ++ " + " ++ show i ++ "/" ++ show (denominator num)

-- | special show dict
showDict :: [(HiValue, HiValue)] -> String
showDict dict = L.intercalate ", " $ map (\(a, b) -> showDots (a, b)) dict
  where
    showDots (a, b) = showP a ++ ": " ++ showP b
    showP = show . prettyValue

-- | not evaluating actions
prettyAction :: HiAction -> Doc AnsiStyle
prettyAction hi = pretty $ case hi of
  HiActionRead path  -> "read(" ++ show path ++ ")"
  HiActionWrite path bytes ->
    L.intercalate "" ["write(", show path, ", ", showByteString bytes, ")"]
  HiActionMkDir path -> "mkdir(" ++ show path ++ ")"
  HiActionChDir path -> "cd(" ++ show path ++ ")"
  HiActionCwd        -> "cwd"
  HiActionNow        -> "now"
  HiActionRand a b   -> "rand(" ++ show a ++ ", " ++ show b ++ ")"
  HiActionEcho text  -> "echo(" ++ show text ++ ")"

-- | show bytes
showByteString :: ByteString -> String
showByteString bytes = "[# " ++ showWords (B.unpack bytes) ++ "#]"

-- | show words
showWords :: (Integral a, Show a) => [a] -> [Char]
showWords (a : as) = plusZero (showHex a "") ++  " " ++ showWords as
showWords [] = ""

-- | fix bug with one hex value
plusZero :: String -> String
plusZero [a] = "0" ++ [a]
plusZero a = a
