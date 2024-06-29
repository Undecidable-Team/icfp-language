{-# LANGUAGE OverloadedStrings #-}

module Parser (module Parser) where

import Types (BiOp (..), Expr (..), Name (..), UnOp (..))

import Control.Monad (void)

import Data.Bifunctor (first)
import Data.Char (chr, ord)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.List (elemIndex)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser :: Type -> Type
type Parser = Parsec Void Text

readExpr :: Text -> Either String Expr
readExpr = first errorBundlePretty . parse parseExpr ""

readWithParser :: Parser a -> Text -> Either String a
readWithParser parser = first errorBundlePretty . parse parser ""

parseExpr :: Parser Expr
parseExpr =
  choice
    [ try parseBool
    , try parseInteger
    , try parseString
    , try parseUnOp
    , try parseBiOp
    , try parseIf
    , try parseLam
    , try parseVar
    ]

parseBool :: Parser Expr
parseBool = VBool <$> parseBoolBody

parseBoolBody :: Parser Bool
parseBoolBody = (True <$ char 'T') <|> (False <$ char 'F')

ctoi :: Char -> Int
ctoi c = ord c - ord '!'

base94Char :: String
base94Char = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

base94String :: String
base94String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n" -- crlf?

parseChar :: Parser Char
parseChar = oneOf base94Char -- maybe use `satisfy`?

parseInteger :: Parser Expr
parseInteger = do
  void (char 'I')
  VInt <$> parseIntegerBody

parseIntegerBody :: Parser Integer
parseIntegerBody = do
  s <- some parseChar
  pure . buildBase94 . map ctoi $ s
 where
  buildBase94 :: [Int] -> Integer
  buildBase94 = foldl' (\acc curr -> acc * 94 + fromIntegral curr) 0

parseString :: Parser Expr
parseString = do
  void (char 'S')
  VString <$> parseStringBody

parseStringBody :: Parser Text
parseStringBody = do
  s <- many parseChar -- not sure: `many` or `some`
  pure . T.pack . map (\c -> base94String !! ctoi c) $ s

printString :: Text -> Text
printString = ("S" <>) . T.map (\c -> maybe '☹' (chr . (+ 33)) $ c `elemIndex` base94String)

parseUnOp :: Parser Expr
parseUnOp = do
  void (char 'U')
  body <- parseUnOpBody
  VUnary body <$> (sc >> parseExpr)

parseUnOpBody :: Parser UnOp
parseUnOpBody = do
  choice
    [ OpNeg <$ char '-'
    , OpNot <$ char '!'
    , OpStringToInt <$ char '#'
    , OpIntToString <$ char '$'
    ]

parseBiOp :: Parser Expr
parseBiOp = do
  void (char 'B')
  body <- parseBiOpBody
  VBinary body <$> (sc >> parseExpr) <*> (sc >> parseExpr)

parseBiOpBody :: Parser BiOp
parseBiOpBody = do
  choice
    [ OpAdd <$ char '+'
    , OpSub <$ char '-'
    , OpMul <$ char '*'
    , OpDiv <$ char '/'
    , OpMod <$ char '%'
    , OpLT <$ char '<'
    , OpGT <$ char '>'
    , OpEQ <$ char '='
    , OpOr <$ char '|'
    , OpAnd <$ char '&'
    , OpConcat <$ char '.'
    , OpTake <$ char 'T'
    , OpDrop <$ char 'D'
    , OpApp <$ char '$'
    ]

parseIf :: Parser Expr
parseIf = do
  void (char '?')
  VIf <$> (sc >> parseExpr) <*> (sc >> parseExpr) <*> (sc >> parseExpr)

parseLam :: Parser Expr
parseLam = do
  void (char 'L')
  VLam <$> (sc >> parseVarBody) <*> (sc >> parseExpr)

parseVar :: Parser Expr
parseVar = do
  void (char 'v')
  VVar <$> parseVarBody

parseVarBody :: Parser Name
parseVarBody = Name <$> parseIntegerBody

sc :: Parser ()
sc = L.space space1 empty empty
