module Main (main) where

import Types
import Parser

import Control.Lens
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.Wreq
import System.Environment
import System.IO

send :: Text -> Text -> IO (Maybe Text)
send tok input = do
  let opts = defaults &
        header "Authorization" .~ [encodeUtf8 $ "Bearer " <> tok]
      body = encodeUtf8 input
  r <- postWith opts "https://boundvariable.space/communicate" body
  pure $ toStrict . decodeUtf8 <$> r ^? responseBody

go :: Text -> IO ()
go tok = do
  T.putStr "> "
  hFlush stdout
  s <- T.getLine
  T.putStrLn ""
  t <- send tok $ printString s
  case readExpr <$> t of
    Nothing -> T.putStrLn "SOMETHING WENT WRONG"
    Just (Left err) -> T.putStrLn "PARSE ERROR: " >> print err
    Just (Right (VString s)) -> T.putStrLn s
    Just (Right ex) -> print ex
  go tok

main :: IO ()
main = do
  tok <- getEnv "AUTH"
  go $ T.pack tok
