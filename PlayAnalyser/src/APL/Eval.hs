module APL.Eval
  ( Val (..),
    Env,
    abstractFixpoint
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)
import Data.Functor.Classes (eq1)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun RangeList VName Exp
  deriving (Eq, Show)


type Range = (Val, Val)

type RangeList = [(VName, Range)]

rangesEmpty :: RangeList
rangesEmpty = []

rangesExtend :: VName -> Val -> RangeList -> RangeList
rangesExtend v val ranges = (v, (val, val)) : ranges

rangesLookup :: VName -> RangeList -> Maybe Range
rangesLookup v ranges = lookup v ranges

type Error = String

newtype EvalM a = EvalM (RangeList -> Either Error a)

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_ranges -> Right x
  (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \ranges ->
    case x ranges of
      Left err -> Left err
      Right x' ->
        let EvalM y = f x'
         in y ranges

askList :: EvalM RangeList
askList = EvalM $ \ranges -> Right ranges

localList :: (RangeList -> RangeList) -> EvalM a -> EvalM a
localList f (EvalM m) = EvalM $ \ranges -> m (f ranges)

failure :: String -> EvalM a
failure s = EvalM $ \_ranges -> Left s

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \ranges ->
  case m1 ranges of
    Left _ -> m2 ranges
    Right x -> Right x

runEval :: EvalM a -> Either Error a
runEval (EvalM m) = m rangesEmpty


-- Returns final stage of fixpoint iteration
abstractFixpoint :: Exp -> RangeList
abstractFixpoint e = do
  -- oldpoint <- []
  case eval e of
    _ -> localList
  


evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = pure $ ValBool True
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp' (div) e1 e2
eval (Pow e1 e2) = evalIntBinOp' (^) e1 e2
eval (Eql e1 e2) = pure $ ValBool True
eval (If cond e1 e2) = eval e2 $ eval e1
eval (Let var e1 e2) = eval e2 $ localList (rangesExtend var (eval e1))
eval (Lambda var body) = do
  ranges <- askList
  pure $ ValFun ranges var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localList (const $ rangesExtend var arg f_env) $ eval body
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2
