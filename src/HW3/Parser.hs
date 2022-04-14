
module HW3.Parser (parse) where

import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import qualified Data.ByteString as B
import Data.Char (digitToInt, isAlpha, isAlphaNum, isHexDigit)
import Data.Text (pack)
import Data.Void (Void)
import Data.Word (Word8)
import HW3.Base (HiAction(..), HiExpr(..), HiFun(..), HiValue)
import HW3.Help (Dep(cons))
import Text.Megaparsec
  (MonadParsec(eof, try), ParseErrorBundle, Parsec, between, choice, many, manyTill, runParser,
  satisfy, (<|>))
import Text.Megaparsec.Char (char, space, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- | funcrion parse or not String to HiExpr
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space *> pExpr <* eof) ""

-- | function for space and comments
sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

number :: (Real a, Num a) => Parser a -> Parser HiExpr
number = fmap (HiExprValue . cons . toRational) . L.signed sc

parens :: Parser HiExpr
parens = between (symbol "(") (symbol ")") pExpr

parensSeq :: Parser HiExpr
parensSeq = between (symbol "[") (symbol "]")
  (HiExprApply (HiExprValue $ cons HiFunList) <$> (try pListBase <|> return []))

parensJSON :: Parser HiExpr
parensJSON = between (symbol "{") (symbol "}")
  (HiExprDict <$> (try parseBaseJSON <|> return []))

parseOneArg :: Parser (HiExpr, HiExpr)
parseOneArg = do
  key <- pExpr
  _ <- symbol ":"
  value <- pExpr
  return (key, value)

parseBaseJSON :: Parser [(HiExpr, HiExpr)]
parseBaseJSON = do
  l <- parseOneArg
  list <- many (symbol "," >> parseOneArg)
  return (l : list)

pCommonFun :: String -> HiFun -> Parser HiExpr
pCommonFun str val = do
  _ <- string str
  return $ HiExprValue $ cons val

pCommon :: String -> HiValue -> Parser HiExpr
pCommon str val = do
  _ <- string str
  return $ HiExprValue val

parensText :: Parser HiExpr
parensText = lexeme $ HiExprValue . cons . pack
            <$> (char '"' >> manyTill L.charLiteral (char '"'))

parensByte :: Parser HiExpr
parensByte = lexeme $ HiExprValue . cons <$> do
  _ <- lexeme $ symbol "[#"
  bytes <- many oneWord8
  _ <- lexeme $ symbol "#]"
  return $ B.pack (bytes)

oneWord8 :: Parser Word8
oneWord8 = do
  a <- lexeme $ satisfy isHexDigit
  b <- satisfy isHexDigit
  lexeme $ return $ fromIntegral (digitToInt a * 16 + digitToInt b)

pTerm :: Parser HiExpr
pTerm = lexeme $ choice
  [ try (number (lexeme L.scientific))
  , (number @Integer) (lexeme L.decimal)
  , parensText
  , parens
  , pCommon    "true"             $ cons True
  , pCommon    "false"            $ cons False
  , pCommon    "null"             $ cons ()
  , parensByte
  , parensSeq
  , parensJSON
  , pCommon    "cwd"              $ cons HiActionCwd
  , pCommon    "now"              $ cons HiActionNow
  , pCommonFun "div"              HiFunDiv
  , pCommonFun "mul"              HiFunMul
  , pCommonFun "add"              HiFunAdd
  , pCommonFun "sub"              HiFunSub
  , pCommonFun "and"              HiFunAnd
  , pCommonFun "or"               HiFunOr
  , pCommonFun "less-than"        HiFunLessThan
  , pCommonFun "greater-than"     HiFunGreaterThan
  , pCommonFun "equals"           HiFunEquals
  , pCommonFun "not-less-than"    HiFunNotLessThan
  , pCommonFun "not-greater-than" HiFunNotGreaterThan
  , pCommonFun "not-equals"       HiFunNotEquals
  , pCommonFun "not"              HiFunNot
  , pCommonFun "if"               HiFunIf
  , pCommonFun "length"           HiFunLength
  , pCommonFun "to-upper"         HiFunToUpper
  , pCommonFun "to-lower"         HiFunToLower
  , pCommonFun "reverse"          HiFunReverse
  , pCommonFun "trim"             HiFunTrim
  , pCommonFun "list"             HiFunList
  , pCommonFun "range"            HiFunRange
  , pCommonFun "fold"             HiFunFold
  , pCommonFun "pack-bytes"       HiFunPackBytes
  , pCommonFun "unpack-bytes"     HiFunUnpackBytes
  , pCommonFun "encode-utf8"      HiFunEncodeUtf8
  , pCommonFun "decode-utf8"      HiFunDecodeUtf8
  , pCommonFun "zip"              HiFunZip
  , pCommonFun "unzip"            HiFunUnzip
  , pCommonFun "serialise"        HiFunSerialise
  , pCommonFun "deserialise"      HiFunDeserialise
  , pCommonFun "read"             HiFunRead
  , pCommonFun "write"            HiFunWrite
  , pCommonFun "mkdir"            HiFunMkDir
  , pCommonFun "cd"               HiFunChDir
  , pCommonFun "parse-time"       HiFunParseTime
  , pCommonFun "rand"             HiFunRand
  , pCommonFun "echo"             HiFunEcho
  , pCommonFun "count"            HiFunCount
  , pCommonFun "keys"             HiFunKeys
  , pCommonFun "values"           HiFunValues
  , pCommonFun "invert"           HiFunInvert
  ]

pListBase :: Parser [HiExpr]
pListBase = do
  l <- pExpr
  list <- many (symbol "," >> pExpr)
  return (l : list)

pList :: Parser [HiExpr]
pList = do
  _ <- symbol "("
  list <- try pListBase <|> return []
  _ <- symbol ")"
  return list

pDot :: Parser [HiExpr]
pDot = do
  _ <- symbol "."
  l <- satisfy isAlpha
  list <- many (satisfy (\c -> (isAlphaNum c) || c == '-'))
  return [HiExprValue . cons . pack $ l : list]

recApply :: HiExpr -> [[HiExpr]] -> HiExpr
recApply fun []       = fun
recApply fun [a]      = HiExprApply fun a
recApply fun (a : ls) = HiExprApply (recApply fun ls) a

pApplyR' :: HiExpr -> Parser HiExpr
pApplyR' fun = do
  lists <- many (try pList <|> pDot)
  run <- symbol "!" <|> return "nope"
  if run == "nope" then
    return $ recApply fun (reverse lists)
  else
    pApplyR' $ HiExprRun $ recApply fun (reverse lists)

pExpr :: Parser HiExpr
pExpr = makeExprParser (pTerm >>= pApplyR') operatorTable

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ listNot
  , [ binN ">=" $ comm HiFunNotLessThan
    , binN "<=" $ comm HiFunNotGreaterThan
    , binN "/=" $ comm HiFunNotEquals
    ]
  , [ binL "*" $ comm HiFunMul
    , binL "/" $ comm HiFunDiv
    ]
  , [ binL "+" $ comm HiFunAdd
    , binL "-" $ comm HiFunSub
    ]
  , [ binN "<" $ comm HiFunLessThan
    , binN ">" $ comm HiFunGreaterThan
    , binN "==" $ comm HiFunEquals
    ]
  , [ binR "&&" $ comm HiFunAnd
    ]
  , [ binR "||" $ comm HiFunOr
    ]
  ]

listNot :: [Operator Parser HiExpr]
listNot = fmap (\a -> binL ([a] ++ "-") $ undefined) ['a' .. 'z']

comm :: HiFun -> HiExpr -> HiExpr -> HiExpr
comm fun a b = HiExprApply (HiExprValue $ cons fun) [a, b]

binL, binN, binR :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binL name f = InfixL (f <$ symbol name)
binN name f = InfixN (f <$ symbol name)
binR name f = InfixR (f <$ symbol name)
