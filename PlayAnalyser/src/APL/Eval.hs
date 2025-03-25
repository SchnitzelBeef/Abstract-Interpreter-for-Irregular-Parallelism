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
  | ValFun Ranges VName Exp
  deriving (Eq, Show)

type UnboundRange = (Val, Val)

type Range = (VName, UnboundRange)

type Ranges = [Range]


getName :: Range -> VName
getName range = fst range

getMin :: Range -> Val
getMin range = fst $ snd range

getMax :: Range -> Val
getMax range = snd $ snd range

rangesEmpty :: Ranges
rangesEmpty = []

double :: Val -> UnboundRange
double val = (val, val)

rangeWiden :: Range -> Ranges -> Ranges
rangeWiden range (range' : ranges) = do
  if getName range == getName range' 
  then (getName range, (min (getMin range) (getMin range'), max (getMax range) (getMax range'))) : ranges
  else range' : rangeWiden range ranges

rangeExtend :: Range -> Ranges -> Ranges
rangeExtend range ranges = do
  case lookup (fst range) ranges of
    Nothing -> range : ranges
    Just _ -> rangeWiden range ranges


rangesUnion :: Ranges -> Ranges -> Ranges
rangesUnion (range : ranges) ranges' = rangesUnion ranges (rangeExtend range ranges')
rangesUnion [] ranges' = ranges'

type Error = String

-- Returns final stage of fixpoint iteration
abstractFixpoint :: Exp -> Ranges
abstractFixpoint e = do
  -- oldpoint <- []
  snd $ eval rangesEmpty  e
  

evalIntBinOp :: Ranges (Integer -> Integer -> Integer) -> Exp -> Exp -> (UnboundRange, Ranges)
evalIntBinOp ranges f e1 e2 = do
  v1 <- eval ranges e1
  v2 <- eval ranges e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> (double $ ValInt $ f x y, rangesEmpty)

eval :: Ranges -> Exp -> (UnboundRange, Ranges)
eval ranges (CstInt x) = (double $ ValInt x, ranges)
eval ranges (CstBool b) = (double $ ValBool b, ranges)
eval ranges (Var v) = do
  case lookup v ranges of
    Right val -> (double $ val, ranges) 
    Left _ -> (double $ ValBool True, ranges) 
eval ranges (Add e1 e2) = evalIntBinOp ranges (+) e1 e2
eval ranges (Sub e1 e2) = evalIntBinOp ranges (-) e1 e2
eval ranges (Mul e1 e2) = evalIntBinOp ranges (*) e1 e2
eval ranges (Div e1 e2) = evalIntBinOp ranges (div) e1 e2
eval ranges (Pow e1 e2) = evalIntBinOp ranges (^) e1 e2
eval ranges (Eql e1 e2) = UnboundRange (ValBool True) (ValBool False)
eval ranges (If cond e1 e2) = do
  case (eval ranges e2, eval ranges e2) of
    (val1, ranges1, val2, ranges2) -> 
      (UnboundRange val1 val2, rangesUnion ranges1 ranges2)
eval ranges (Let var e1 e2) = do
  (unboundRange, ranges') <- eval ranges e1 
  boundRange <- (var, unboundRange) 
  newRanges <- rangeExtend boundRange ranges'
  eval ranges' e2 newRanges


-- eval ranges (Lambda var body) = do
--   ranges <- askList
--   pure $ ValFun ranges var body
-- eval ranges (Apply e1 e2) = do
--   v1 <- eval e1
--   v2 <- eval e2
--   case (v1, v2) of
--     (ValFun f_env var body, arg) ->
--       localList (const $ rangesExtend var arg f_env) $ eval body
-- eval ranges (TryCatch e1 e2) =
--   eval e1 `catch` eval e2
