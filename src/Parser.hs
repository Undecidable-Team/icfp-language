{-# LANGUAGE OverloadedStrings #-}

module Parser (module Parser) where

import Types (BiOp (..), Expr (..), Name (..), UnOp (..))

import Control.Monad (void)

import Data.Char (ord, chr)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.List (elemIndex)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser :: Type -> Type
type Parser = Parsec Void Text

parseExpr :: Parser Expr
parseExpr =
  choice
    [ try (VBool <$> parseBool)
    , try (char 'I' >> (VInt <$> parseInteger))
    , try (char 'S' >> (VString <$> parseString))
    , try (char 'U' >> (VUnary <$> parseUnOp <*> (sc >> parseExpr)))
    , try (char 'B' >> (VBinary <$> parseBiOp <*> (sc >> parseExpr) <*> (sc >> parseExpr)))
    , try (char '?' >> (VIf <$> (sc >> parseExpr) <*> (sc >> parseExpr) <*> (sc >> parseExpr)))
    , try (char 'L' >> (VLam <$> (sc >> parseName) <*> (sc >> parseExpr)))
    , try (char 'v' >> (VVar <$> parseName))
    ]

parseBool :: Parser Bool
parseBool = (True <$ char 'T') <|> (False <$ char 'F')

ctoi :: Char -> Int
ctoi c = ord c - ord '!'

base94Char :: String
base94Char = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

base94String :: String
base94String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n" -- crlf?

parseChar :: Parser Char
parseChar = oneOf base94Char -- maybe use `satisfy`?

parseInteger :: Parser Integer
parseInteger = do
  s <- some parseChar
  pure . buildBase94 . map ctoi $ s
 where
  buildBase94 :: [Int] -> Integer
  buildBase94 = foldl' (\acc curr -> acc * 94 + fromIntegral curr) 0

parseString :: Parser Text
parseString = do
  s <- many parseChar -- not sure: `many` or `some`
  pure . T.pack . map (\c -> base94String !! ctoi c) $ s

printString :: Text -> Text
printString = ("S" <>) . T.map (\c -> maybe '☹' (chr . (+33)) $ c `elemIndex` base94String)

parseUnOp :: Parser UnOp
parseUnOp = do
  choice
    [ OpNeg <$ char '-'
    , OpNot <$ char '!'
    , OpStringToInt <$ char '#'
    , OpIntToString <$ char '$'
    ]

parseBiOp :: Parser BiOp
parseBiOp = do
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

parseName :: Parser Name
parseName = Name <$> parseInteger

sc :: Parser ()
sc = L.space space1 empty empty
