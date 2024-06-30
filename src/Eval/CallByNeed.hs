module Eval.CallByNeed (module Eval.CallByNeed) where

import Parser (base94String)
import Types (BiOp (..), Expr (..), Name (..), UnOp (..))

import Control.Applicative (Applicative (liftA2))

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)

type Env :: Type
type Env = Map Name Expr

type EvalError :: Type
data EvalError
  = TypeError String Expr
  | UnboundVariable Name
  | UndefinedBehavior
  deriving stock (Show)

strToInt :: Text -> Integer
strToInt = T.foldl step 0
 where
  step x c = x * 94 + toInteger (idx c)
  idx c = fromMaybe (error "Wrong symbol") $ elemIndex c base94String

intToStr :: Integer -> Text
intToStr = T.reverse . T.unfoldr step
 where
  step = \case
    0 -> Nothing
    x -> Just (base94String !! fromInteger (x `mod` 94), x `div` 94)

evalWithEnv :: Env -> Expr -> Either EvalError Expr
evalWithEnv env expr = case expr of
  VBool _ -> pure expr
  VInt _ -> pure expr
  VString _ -> pure expr
  VUnary op e -> evalWithEnvUnOp env op e
  VBinary OpApp ea eb ->
    evalWithEnv env ea >>= \case
      VLam name body -> evalWithEnv (M.insert name eb env) body
      expr' -> Left $ TypeError "Not a function" expr'
  VBinary op ea eb -> do
    ea' <- evalWithEnv env ea
    eb' <- evalWithEnv env eb
    evalWithEnvBin env op ea' eb'
  VIf cond conseq alt -> evalWithEnvBool env cond >>= \c -> if c then evalWithEnv env conseq else evalWithEnv env alt
  VLam _ _ -> pure expr
  VVar name -> maybe (Left $ UnboundVariable name) pure (M.lookup name env)
  VOther -> Left UndefinedBehavior

evalWithEnvUnOp :: Env -> UnOp -> Expr -> Either EvalError Expr
evalWithEnvUnOp env op expr =
  evalWithEnv env expr >>= \r -> case (op, r) of
    (OpNeg, VInt n) -> pure . VInt . negate $ n
    (OpNot, VBool b) -> pure . VBool . not $ b
    (OpStringToInt, VString s) -> pure . VInt . strToInt $ s
    (OpIntToString, VInt n) -> pure . VString . intToStr $ n
    _ -> Left $ TypeError "Not an unary operation" (VUnary op expr)

evalWithEnvBin :: Env -> BiOp -> Expr -> Expr -> Either EvalError Expr
evalWithEnvBin env op ea eb = do
  lhs <- evalWithEnv env ea
  rhs <- evalWithEnv env eb
  case (op, lhs, rhs) of
    (OpAdd, VInt na, VInt nb) -> pure $ VInt (na + nb)
    (OpSub, VInt na, VInt nb) -> pure $ VInt (na - nb)
    (OpMul, VInt na, VInt nb) -> pure $ VInt (na * nb)
    (OpDiv, VInt na, VInt nb) -> pure $ VInt (na `quot` nb)
    (OpMod, VInt na, VInt nb) -> pure $ VInt (na `rem` nb)
    (OpLT, VInt na, VInt nb) -> pure $ VBool (na < nb)
    (OpGT, VInt na, VInt nb) -> pure $ VBool (na > nb)
    (OpEQ, va, vb) -> pure $ VBool (va == vb)
    (OpOr, VBool ba, VBool bb) -> pure $ VBool (ba || bb)
    (OpAnd, VBool ba, VBool bb) -> pure $ VBool (ba && bb)
    (OpConcat, VString sa, VString sb) -> pure $ VString (sa <> sb)
    (OpTake, VInt n, VString s) -> pure $ VString (T.take (fromInteger n) s)
    (OpDrop, VInt n, VString s) -> pure $ VString (T.drop (fromInteger n) s)
    (op', ea', eb') -> Left $ TypeError "Unsupported binary operation" (VBinary op' ea' eb')

evalWithEnvBool :: Env -> Expr -> Either EvalError Bool
evalWithEnvBool env expr = case expr of
  VBool b -> pure b
  VUnary op ea -> case op of
    OpNot -> not <$> evalWithEnvBool env ea
    _ -> Left $ TypeError "Unsupported Boolean unary operator." (VUnary op ea)
  VBinary op ea eb -> case op of
    OpAnd -> liftA2 (&&) (evalWithEnvBool env ea) (evalWithEnvBool env eb)
    OpOr -> liftA2 (||) (evalWithEnvBool env ea) (evalWithEnvBool env eb)
    OpLT -> liftA2 (<) (evalWithEnvInt env ea) (evalWithEnvInt env eb)
    OpGT -> liftA2 (>) (evalWithEnvInt env ea) (evalWithEnvInt env eb)
    OpEQ -> liftA2 (==) (evalWithEnv env ea) (evalWithEnv env eb)
    _ -> Left $ TypeError "Unsupported Boolean binary operator." (VBinary op ea eb)
  _ -> Left $ TypeError "Invalid boolean expression." expr

evalWithEnvInt :: Env -> Expr -> Either EvalError Integer
evalWithEnvInt env expr =
  evalWithEnv env expr >>= \case
    VInt n -> pure n
    expr' -> Left $ TypeError "Not an integer" expr'
