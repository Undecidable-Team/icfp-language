-- | Types for Lambda

module Types where

import Data.Text (Text)

data Expr
  = VBool Bool
  | VInt Integer
  | VString Text
  | VUnary UnOp Expr
  | VBinary BiOp Expr Expr
  | VIf Expr Expr Expr
  | VLam Name Expr
  | VVar Name
  | VOther -- for the future
  deriving (Eq,Show,Read)

newtype Name = Name { unName :: Integer }
  deriving (Eq,Show,Read)

data UnOp
  = OpNeg
  | OpNot
  | OpStringToInt
  | OpIntToString
  deriving (Eq,Show,Read)

data BiOp
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  | OpLT
  | OpGT
  | OpEQ
  | OpOr
  | OpAnd
  | OpConcat
  | OpTake
  | OpDrop
  | OpApp
  deriving (Eq,Show,Read)
