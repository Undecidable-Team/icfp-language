module Eval.CallByNeed (module Eval.CallByNeed) where

import Parser (base94String)
import Types (BiOp (..), Expr (..), Name (..), UnOp (..))

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (StateT (runStateT), get)

import Data.Kind (Type)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Hashtables qualified as H
import Data.Vector.Mutable qualified as VM

type Thunk :: Type
data Thunk = Unevaluated Expr | Evaluated Expr

type HashTable :: Type -> Type -> Type
type HashTable k v = H.Dictionary (H.PrimState IO) VM.MVector k VM.MVector v

type Env :: Type
type Env = HashTable Integer Thunk

type EvalError :: Type
data EvalError
  = TypeError String Expr
  | UnboundVariable Name
  | UndefinedBehavior
  deriving stock (Show)

type EvalM :: Type -> Type
type EvalM a = ExceptT EvalError (StateT Env IO) a

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

evalWithEnv :: Expr -> EvalM Expr
evalWithEnv expr = case expr of
  VBool _ -> pure expr
  VInt _ -> pure expr
  VString _ -> pure expr
  VUnary op e -> evalWithEnvUnOp op e
  VBinary OpApp ea eb ->
    evalWithEnv ea >>= \case
      VLam name body -> do
        env <- get
        _ <- H.insert env (unName name) (Unevaluated eb)
        evalWithEnv body
      expr' -> throwError $ TypeError "Not a function" expr'
  VBinary op ea eb -> do
    ea' <- evalWithEnv ea
    eb' <- evalWithEnv eb
    evalWithEnvBin op ea' eb'
  VIf cond conseq alt -> evalWithEnvBool cond >>= \c -> if c then evalWithEnv conseq else evalWithEnv alt
  VLam _ _ -> pure expr
  VVar name -> do
    env <- get
    H.lookup env (unName name) >>= \case
      Just (Unevaluated e) -> do
        e' <- evalWithEnv e
        env' <- get
        _ <- H.insert env' (unName name) (Evaluated e')
        pure e'
      Just (Evaluated e) -> pure e
      Nothing -> throwError $ UnboundVariable name
  VOther -> throwError UndefinedBehavior

evalWithEnvUnOp :: UnOp -> Expr -> EvalM Expr
evalWithEnvUnOp op expr =
  evalWithEnv expr >>= \r -> case (op, r) of
    (OpNeg, VInt n) -> pure . VInt . negate $ n
    (OpNot, VBool b) -> pure . VBool . not $ b
    (OpStringToInt, VString s) -> pure . VInt . strToInt $ s
    (OpIntToString, VInt n) -> pure . VString . intToStr $ n
    _ -> throwError $ TypeError "Not an unary operation" (VUnary op expr)

evalWithEnvBin :: BiOp -> Expr -> Expr -> EvalM Expr
evalWithEnvBin op ea eb = do
  lhs <- evalWithEnv ea
  rhs <- evalWithEnv eb
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
    (op', ea', eb') -> throwError $ TypeError "Unsupported binary operation" (VBinary op' ea' eb')

evalWithEnvBool :: Expr -> EvalM Bool
evalWithEnvBool expr = case expr of
  VBool b -> pure b
  VUnary op ea -> case op of
    OpNot -> not <$> evalWithEnvBool ea
    _ -> throwError $ TypeError "Unsupported Boolean unary operator." (VUnary op ea)
  VBinary op ea eb -> case op of
    OpAnd -> liftA2 (&&) (evalWithEnvBool ea) (evalWithEnvBool eb)
    OpOr -> liftA2 (||) (evalWithEnvBool ea) (evalWithEnvBool eb)
    OpLT -> liftA2 (<) (evalWithEnvInt ea) (evalWithEnvInt eb)
    OpGT -> liftA2 (>) (evalWithEnvInt ea) (evalWithEnvInt eb)
    OpEQ -> liftA2 (==) (evalWithEnv ea) (evalWithEnv eb)
    _ -> throwError $ TypeError "Unsupported Boolean binary operator." (VBinary op ea eb)
  _ -> throwError $ TypeError "Invalid boolean expression." expr

evalWithEnvInt :: Expr -> EvalM Integer
evalWithEnvInt expr =
  evalWithEnv expr >>= \case
    VInt n -> pure n
    expr' -> throwError $ TypeError "Not an integer" expr'

eval :: Expr -> IO (Either EvalError Expr)
eval expr = do
  h <- (H.initialize 0 :: IO Env)
  (r, _) <- runStateT (runExceptT (evalWithEnv expr)) h
  pure r
