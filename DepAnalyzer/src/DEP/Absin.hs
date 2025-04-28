module DEP.Absin
  ( RangeVal (..),
    Env,
    ranges,
    runRanges,
    envIntersect,
    freeVNames,
    Error
  )
where

import Control.Monad (ap, liftM)
import DEP.AST (Exp (..), VName)

type Range = (Int, Int)

-- An invariant to the RangeTuple, is that they are always sorted based on the end of the range
-- I.e. RangeTuple [Range 1 4, Range 100 200] is valid, but RangeTuple [Range 1, 4, Range -1, 0] isn't
data RangeVal = RangeTop
  | RangeBottom
  | Ranges [Range]    -- all the ranges the value can take in a single variable, so e.g. Ranges [(1, 2), (6, 9)]
  | RangeTuple [RangeVal]  -- the ranges of the actual tuple value type, so, RangeTuple [RangeTop, Ranges [(1, 5)]]
  | RangeFun Env VName Exp 
  deriving (Eq, Show)

type Error = String

type Env = [(VName, RangeVal)]

envEmpty :: Env
envEmpty = []

envLookup :: VName -> Env -> Maybe RangeVal
envLookup v env = lookup v env

runRanges :: Env -> EvalM a -> Either Error a
runRanges env (EvalM m) = m env

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

{- Creates an abstract interpretation of the current expression and returns the expression can take-}
ranges :: Exp -> EvalM RangeVal
ranges (CstInt val) = pure $ Ranges [(val, val)]

depVals :: 