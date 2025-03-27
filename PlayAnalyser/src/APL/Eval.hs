module APL.Eval
  ( Val (..),
    Env,
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

{- "Val" datatype consists of "sets" of booleans and ranges of integers or None-}
data Val
  = ValBool Bool
  | ValFun Env VName Exp
  | ValIntRange Integer Integer 
  | ValBoolBoth                 
  | None                        
  deriving (Eq, Show)

eval' :: Env -> Exp -> Either Error Val
eval' env = runEval env . eval

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

newtype EvalM a = EvalM (Env -> Either Error a)

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_env -> Right x
  (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env ->
    case x env of
      Left err -> Left err
      Right x' ->
        let EvalM y = f x'
         in y env

askEnv :: EvalM Env
askEnv = EvalM $ \env -> Right env

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

failure :: String -> EvalM a
failure s = EvalM $ \_env -> Left s

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env ->
  case m1 env of
    Left _ -> m2 env
    Right x -> Right x

runEval :: Env -> EvalM a -> Either Error a
runEval env (EvalM m) = m env

{- | Compares two ranges to check for complete equality -}
rangeCompare :: Val -> Val -> Bool
rangeCompare val1 val2 = 
  case (val1, val2) of
    (ValIntRange int1 int2, ValIntRange int3 int4) -> int1 == int3 && int2 == int4
    (ValBool x, ValBool y) -> x == y
    (ValBoolBoth, ValBoolBoth) -> True
    (None, None) -> True
    (_, _) -> False -- Could throw error instead

{- | Intersects two ranges -}
rangeIntersect :: Exp -> Exp -> EvalM Val
rangeIntersect e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValBoolBoth, ValBool y) -> pure $ ValBool y
    (ValBool x, ValBoolBoth) -> pure $ ValBool x
    (ValBool x, ValBool y) -> if x == y
      then pure $ ValBool x else pure None
    (ValIntRange int1 int2, ValIntRange int3 int4) -> if int2 >= int3 && int4 >= int1 -- Normal rectangle 2d collision
      then pure $ ValIntRange (max int1 int3) (min int2 int4) else pure None
    (None, _) -> pure None
    (_, None) -> pure None
    (_, _) -> failure "Incompatible types in rangeIntersect"

{- | Unions two ranges -}
rangeUnion :: Exp -> Exp -> EvalM Val
rangeUnion e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValBoolBoth, ValBool _) -> pure ValBoolBoth 
    (ValBool _, ValBoolBoth) -> pure ValBoolBoth 
    (ValBool x, ValBool y) -> if x /= y
      then pure ValBoolBoth else pure $ ValBool x
    (ValIntRange int1 int2, ValIntRange int3 int4) -> pure $ ValIntRange (min int1 int3) (max int2 int4)
    (None, y) -> pure y
    (x, None) -> pure x
    (_, _) -> failure "Incompatible types rangeUnion"


evalIntBinOp :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValIntRange int1 int2, ValIntRange int3 int4) -> 
      let vals = [f int1 int3, f int1 int4, f int2 int3, f int2 int4]
      in pure $ ValIntRange (minimum vals) (maximum vals)
    (_, _) -> failure "Non-integer operand"

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValIntRange x x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp (*) e1 e2
eval (Div e1 e2) = do
  v <- eval e2
  case v of
    (ValIntRange int1 int2) ->
      if int1 <= 0 && int2 >= 0
        then pure None
        else evalIntBinOp div e1 e2
    _ -> failure "Non-int range in division"
eval (Pow e1 e2) = do
  v <- eval e2
  case v of
    (ValIntRange int1 int2) ->
      if int1 < 0
        then pure None
        else evalIntBinOp (^) e1 e2
    _ -> failure "Non-int range in power"
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y -- Potential complete overlap
    (ValBoolBoth, ValBool _) -> pure ValBoolBoth      -- Overlapping values
    (ValBool _, ValBoolBoth) -> pure ValBoolBoth      -- Overlapping values
    (ValBoolBoth, ValBoolBoth) -> pure ValBoolBoth    -- Overlapping values
    (ValIntRange int1 int2, ValIntRange int3 int4) -> if int1 == int4 && int2 == int3
      then pure $ ValBool True             -- One element complete overlap
      else if int2 >= int3 && int4 >= int1 -- Normal rectangle 2d collision
        then pure ValBoolBoth              -- Overlapping ranges
        else pure $ ValBool False          -- Non-overlapping ranges
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    ValBoolBoth -> rangeUnion e1 e2        -- Both conditions possible? Union!
    _ -> failure "Non-boolean conditional."
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2
eval (RandomInt e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValIntRange int1 int2, ValIntRange int3 int4) -> pure $ ValIntRange (min int1 int3) (max int3 int4)
    _ -> failure "Non-integer found in RandomInt."
eval (Loop p pe i ie body) = do -- Hideous code for for-loop (sorry): 
  v1 <- eval ie
  v2 <- eval pe
  case v1 of
    ValIntRange int1 int2 -> 
      case (runEval [] (loop v2 (0, int1)), runEval [] (loop v2 (0, int2))) of
        (Right (ValIntRange v1' v1''), Right (ValIntRange v2' v2'')) -> pure $ ValIntRange (min v1' v2') (max v1'' v2'') 
        (Left err1, Left err2) -> failure $ err1 ++ err2  -- *Should* be impossible :-)
    _ -> failure "Non-valid loop termination"
  where loop acc (j, n) = do
          v3 <- localEnv (const $ envExtend i (ValIntRange j j) (envExtend p acc envEmpty)) $ eval body 
          if j < n
            then loop v3 (j+1, n)
            else pure acc
