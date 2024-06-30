module Eval (module Eval) where

import Types (BiOp (..), Expr (..), Name (..), UnOp (..))

import Data.Kind (Type)
import Data.List (elemIndex)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- Evaluation of Expressions

-- Variable Environment (Typing Context)
type Env :: Type
type Env = Map Name Expr

-- Helper functions for Evaluation

-- Evaluates Boolean expressions

evalBool :: Env -> Expr -> Bool
evalBool env expr = case expr of
  VBool b -> b
  VUnary op e1 -> case op of
    OpNot -> not (evalBool env e1)
    _ -> error "Unsupported Boolean unary operator."
  VBinary op e1 e2 -> case op of
    OpAnd -> evalBool env e1 && evalBool env e2
    OpOr -> evalBool env e1 || evalBool env e2
    OpLT -> evalInt env e1 < evalInt env e2
    OpGT -> evalInt env e1 > evalInt env e2
    OpEQ -> eval env e1 == eval env e2
    _ -> error "Unsupported Boolean operator."
  _ -> error "Invalid boolean expression."

-- Evaluates Integer expressions
evalInt :: Env -> Expr -> Integer -- Note that `Int' only uses fixed-precision ones, while `Integer' type has arbitrary-precision.
evalInt env e = case eval env e of
  VInt n -> n
  _ -> error "Invalid type: This is not an integer expression!"

-- Evaluates Unary Operations
evalUn :: Env -> UnOp -> Expr -> Expr
evalUn env op expr = case (op, eval env expr) of
  (OpNeg, VInt n) -> VInt (-n)
  (OpNot, VBool b) -> VBool (not b)
  (OpStringToInt, VString s) -> VInt (strToInt s)
  (OpIntToString, VInt n) -> VString (intToStr n)
  _ -> error "Invalid type: Not an unary operation."

strToInt :: Text -> Integer
strToInt = T.foldl step 0
 where
  step x c = x * 94 + toInteger (idx c)
  alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"
  idx c = fromMaybe (error "Wrong symbol") $ elemIndex c alphabet

intToStr :: Integer -> Text
intToStr = T.reverse . T.unfoldr step
 where
  step = \case
    0 -> Nothing
    x -> Just (alphabet !! fromInteger (x `mod` 94), x `div` 94)
  alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

-- Evaluates Binary Operations
evalBin :: Env -> BiOp -> Expr -> Expr -> Expr
evalBin env op e1 e2 = case (op, eval env e1, eval env e2) of
  (OpAdd, VInt n1, VInt n2) -> VInt (n1 + n2)
  (OpSub, VInt n1, VInt n2) -> VInt (n1 - n2)
  (OpMul, VInt n1, VInt n2) -> VInt (n1 * n2)
  (OpDiv, VInt n1, VInt n2) -> VInt (n1 `quot` n2)
  (OpMod, VInt n1, VInt n2) -> VInt (n1 `rem` n2)
  (OpLT, VInt n1, VInt n2) -> VBool (n1 < n2)
  (OpGT, VInt n1, VInt n2) -> VBool (n1 > n2)
  (OpEQ, e1', e2') -> VBool (e1' == e2')
  (OpOr, VBool b1, VBool b2) -> VBool (b1 || b2)
  (OpAnd, VBool b1, VBool b2) -> VBool (b1 && b2)
  (OpConcat, VString s1, VString s2) -> VString (s1 <> s2)
  (OpTake, VInt n, VString s) -> VString (T.take (fromInteger n) s)
  (OpDrop, VInt n, VString s) -> VString (T.drop (fromInteger n) s)

-- The main evaluation funciton

subst :: Name -> Expr -> Expr -> Expr
subst name v = \case
  VBool b -> VBool b
  VInt i -> VInt i
  VString s -> VString s
  VUnary op e -> VUnary op $ subst name v e
  VBinary op e1 e2 -> VBinary op (subst name v e1) (subst name v e2)
  VIf c t e -> VIf (subst name v c) (subst name v t) (subst name v e)
  VLam n b
    | n == name -> VLam n b
    | otherwise -> VLam n (subst name v b)
  VVar n
    | n == name -> v
    | otherwise -> VVar n
  VOther -> VOther

eval :: Env -> Expr -> Expr
eval env expr = case expr of
  VBool b -> VBool b
  VInt n -> VInt n
  VVar name -> VVar name
  VLam name body -> VLam name body
  VIf cond t e -> if evalBool env cond then eval env t else eval env e
  VString s -> VString s
  VUnary op e -> evalUn env op (eval env e)
  VBinary OpApp e1 e2 -> case eval env e1 of
    VLam name body -> let body' = subst name e2 body in eval env body'
    _ -> error "Invalid type: Applied function is not valid lambda expression"
  VBinary op e1 e2 -> evalBin env op (eval env e1) (eval env e2)
  VOther -> error "Expression type not supported."
