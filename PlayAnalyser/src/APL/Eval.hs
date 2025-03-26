module APL.Eval
  ( Val (..),
    Env,
    envEmpty,
    runEval,
    abstractFixpoint
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)
import Data.Functor.Classes (eq1)

data Val
  = ValInt Integer
  | ValBool Bool
  -- | ValFun Ranges VName Exp
  deriving (Eq, Show)

type UnboundRanges = [(Val, Val)]

type BoundRange = (VName, UnboundRanges)

type Env = [BoundRange]

envEmpty :: Env
envEmpty = []

envAppend :: VName -> Env -> Env
envAppend v env = (v, []) : env 

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

runEval :: EvalM a -> Either Error a
runEval (EvalM m) = m envEmpty

-- getName :: Range -> VName
-- getName range = fst range

-- getMin :: Range -> Val
-- getMin range = fst $ snd range

-- getMax :: Range -> Val
-- getMax range = snd $ snd range

-- double :: Val -> UnboundRange
-- double val = (val, val)

-- rangeWiden :: Range -> Ranges -> Ranges
-- rangeWiden range (range' : ranges) = do
--   if getName range == getName range' 
--   then (getName range, (min (getMin range) (getMin range'), max (getMax range) (getMax range'))) : ranges
--   else range' : rangeWiden range ranges

-- rangeExtend :: Range -> Ranges -> Ranges
-- rangeExtend range ranges = do
--   case lookup (fst range) ranges of
--     Nothing -> range : ranges
--     Just _ -> rangeWiden range ranges


-- rangesUnion :: Ranges -> Ranges -> Ranges
-- rangesUnion (range : ranges) ranges' = rangesUnion ranges (rangeExtend range ranges')
-- rangesUnion [] ranges' = ranges'

type Error = String

locateidentifiers :: Exp -> Env
locateidentifiers (Var v) = envEmpty
locateidentifiers (Let v e1 e2) = (envAppend v envEmpty) ++ (locateidentifiers e1) ++ (locateidentifiers e2) 
locateidentifiers (Lambda v e1) = (envAppend v envEmpty) ++ (locateidentifiers e1)
locateidentifiers (CstInt x) = envEmpty
locateidentifiers (CstBool b) = envEmpty
locateidentifiers (Add e1 e2) = (locateidentifiers e1) ++ (locateidentifiers e2)
locateidentifiers (Sub e1 e2) = (locateidentifiers e1) ++ (locateidentifiers e2)
locateidentifiers (Mul e1 e2) = (locateidentifiers e1) ++ (locateidentifiers e2)
locateidentifiers (Div e1 e2) = (locateidentifiers e1) ++ (locateidentifiers e2)
locateidentifiers (Pow e1 e2) = (locateidentifiers e1) ++ (locateidentifiers e2)
locateidentifiers (Eql e1 e2) = (locateidentifiers e1) ++ (locateidentifiers e2)
locateidentifiers (If cond e1 e2) = (locateidentifiers e1) ++ (locateidentifiers e2)
locateidentifiers (Apply e1 e2) = (locateidentifiers e1) ++ (locateidentifiers e2)
-- eval ranges (TryCatch e1 e2) = 

-- -- Returns final stage of fixpoint iteration
abstractFixpoint :: Exp -> Env
abstractFixpoint e = do
  locateidentifiers e $ map (\v -> eval v)
  askEnv

  
eval :: Env -> Exp -> Env
eval env (CstInt x) = ValInt x
eval env (CstBool b) = ValBool b
eval env (Var v) = 
  do
  case lookup v env of
    Right val -> (double $ val, env) 
    Left _ -> (double $ ValBool True, env) 
eval env (Add e1 e2) = evalIntBinOp env (+) e1 e2
eval env (Sub e1 e2) = evalIntBinOp env (-) e1 e2
eval env (Mul e1 e2) = evalIntBinOp env (*) e1 e2
eval env (Div e1 e2) = evalIntBinOp env (div) e1 e2
eval env (Pow e1 e2) = evalIntBinOp env (^) e1 e2
eval env (Eql e1 e2) = ((ValBool True) (ValBool False), env)
eval env (If cond e1 e2) = do

  case (eval env e2, eval env e2) of
    (val1, ranges1, val2, ranges2) -> 
      (val1 val2, rangesUnion ranges1 ranges2)
eval env (Let var e1 e2) = do
  e1' <- eval state e1 
  eval (updateState env e1') e2
eval env (Lambda var body) = do
  env <- askList
  pure $ ValFun env var body
eval env (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localList (const $ envExtend var arg f_env) $ eval body
eval env (TryCatch e1 e2) =
  eval e1 `catch` eval e2




-- evalIntBinOp :: Ranges -> (Integer -> Integer -> Integer) -> Exp -> Exp -> (UnboundRange, Ranges)
-- evalIntBinOp ranges f e1 e2 = do
--   v1 <- eval ranges e1
--   v2 <- eval ranges e2
--   case (v1, v2) of
--     ((ValInt x, _), (ValInt y, _)) -> (double $ ValInt $ f x y, rangesEmpty)

-- eval :: Ranges -> Exp -> (UnboundRange, Ranges)
-- eval ranges (CstInt x) = (double $ ValInt x, ranges)
-- eval ranges (CstBool b) = (double $ ValBool b, ranges)
-- eval ranges (Var v) = do
--   case lookup v ranges of
--     Right val -> (double $ val, ranges) 
--     Left _ -> (double $ ValBool True, ranges) 
-- eval ranges (Add e1 e2) = evalIntBinOp ranges (+) e1 e2
-- eval ranges (Sub e1 e2) = evalIntBinOp ranges (-) e1 e2
-- eval ranges (Mul e1 e2) = evalIntBinOp ranges (*) e1 e2
-- eval ranges (Div e1 e2) = evalIntBinOp ranges (div) e1 e2
-- eval ranges (Pow e1 e2) = evalIntBinOp ranges (^) e1 e2
-- eval ranges (Eql e1 e2) = ((ValBool True) (ValBool False), ranges)
-- eval ranges (If cond e1 e2) = do
--   case (eval ranges e2, eval ranges e2) of
--     (val1, ranges1, val2, ranges2) -> 
--       (val1 val2, rangesUnion ranges1 ranges2)
-- eval ranges (Let var e1 e2) = do
--   (unboundRange, ranges') <- eval ranges e1 
--   boundRange <- (var, unboundRange) 
--   newRanges <- rangeExtend boundRange ranges'
--   eval ranges' e2 newRanges
-- -- eval ranges (Lambda var body) = do
-- --   ranges <- askList
-- --   pure $ ValFun ranges var body
-- -- eval ranges (Apply e1 e2) = do
-- --   v1 <- eval e1
-- --   v2 <- eval e2
-- --   case (v1, v2) of
-- --     (ValFun f_env var body, arg) ->
-- --       localList (const $ rangesExtend var arg f_env) $ eval body
-- -- eval ranges (TryCatch e1 e2) =
-- --   eval e1 `catch` eval e2
