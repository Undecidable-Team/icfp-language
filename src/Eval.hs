-- Evaluation of Expressions

-- Variable Environment (Typing Context)
type Env = [(Name, Expr)]

-- Helper functions for Evaluation

-- Evaluates Boolean expressions

evalBool :: Env -> Expr -> Bool
evalBool env expr = case expr of
  VBool b	-> b
  VBinary op e1 e2 -> case op of
    			OpAnd	-> evalBool env e1 && evalBool env e2
                        OpOr	-> evalBool env e1 || evalBool env e2
                        OpNot	-> not (evalBool env e1)
                        OpLT	-> evalInt env e1 < evalInt env e2
                        OpGT	-> evalInt env e1 > evalInt env e2
                        OpEQ	-> eval env e1 == eval env e2
                        _	-> error "Unsupported Boolean operator."
evalBool _ _ = error "Invalid boolean expression."

-- Evaluates Integer expressions
evalInt :: Env -> Expr -> Integer -- Note that `Int' only uses fixed-precision ones, while `Integer' type has arbitrary-precision.
evalint env (VInt n) = n
evalInt _ _ = error "Invalid type: This is not an integer expression!"

-- Evaluates Unary Operations
evalUn :: Env -> UnOp -> Expr -> Expr
evalUn env op expr = case (op, eval env expr) of
  (OpNeg, VInt n)	-> VInt (-n)
  (OpNot, VBool b)	-> VBool (not b)
  (OpStringToInt, VString s) -> VInt (strToInt s)
  (OpIntToString, VInt n) -> VString (intToStr n)
  _			-> error "Invalid type: Not an unary operation."

strToInt :: String -> Integer
strToInt = -- TODO

intToStr :: Integer -> String
intToSTr = -- TODO

-- Evaluates Binary Operations
evalBin :: Env -> BiOp -> Expr -> Expr -> Expr -> Expr
evalBin env op e1 e2 = case (op, e1, e2) of
  (OpAdd, VInt n1, VInt n2)	-> VInt (n1 + n2)
  (OpAdd, VInt n1, VInt n2)	-> VInt (n1 - n2)
  (OpMul, VInt n1, VInt n2)	-> VInt (n1 * n2)
  (OpDiv, VInt n1, VInt n2)	-> VInt (n1 / n2)
  (OpMod, VInt n1, VInt n2)	-> VInt (n1 `mod` n2)
  (OpLT, VInt n1, VInt n2)	-> VInt (n1 < n2)
  (OpGT, VInt n1, VInt n2)	-> VInt (n1 > n2)
  (OpEQ, e1, e2)	-> VBool (e1 == e2)
  (OpOr, VBool b1, VBool b2)	-> VBool (b1 || b2)
  (OpAnd, VBool b1, VBool b2)	-> VBool (b1 && b2)
  (OpConcat, VString s1, VString s2) -> VString (s1 ++ s2)
  (OpTake, VInt n, VString s) -> VString (take n s)
  (OpDrop, VInt n, VString s) -> VString (drop n s)
  (OpApp, VLam name t body, arg) -> eval (ExtendEnv env name arg) body
  (OnApp, f, x) ->
    let f = eval env f
        x = eval env x
     in case f of
      	 VLam name t body -> eval (extendEnv env name arg) body
         		  -> error "Invalid type: Applied function is not valid lambda expression"

-- Looking up variables in environment
lookupEnv :: Env -> Name -> Expr
lookupEnv [] name	= error ("Unbound variable: " ++ show name)
lookupEnv ((n, v) : env) name
	| n == name = v
        | otherwise = lookupEnv name

-- Adding more variable bindings to environment
extendEnv :: Env -> name -. Expr -> Env
extendEnv env name value = (name, value) : env


-- The main evaluation funciton

eval :: Env -> Expr -> Expr
eval env expr = case expr of
  VBool b -> VBool b
  VInt n -> VInt n
  VVar name -> lookupEnv env name
  VLam name t body -> VLam name t body
  VApp e1 e2 -> case eval env e1 of
                  VLam name t body -> eval (extendEnv env name e2) body
                                   -> error "Invalid type: Not an application."
  VIf cond t e      -> if evalBool env cond then eval env t else eval env e
  VString s -> VString s
  VUnary op e -> evalUn env op e
  VBinary op e1 e2 -> evalBin env op (eval env e1) (eval env e2)
  VOther -> error "Expression type not supported."
