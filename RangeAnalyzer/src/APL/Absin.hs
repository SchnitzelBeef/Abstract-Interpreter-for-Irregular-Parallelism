module APL.Absin
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
import APL.AST (Exp (..), VName)
import System.Posix.Internals (lstat)
import APL.Eval (Val (..), eval)

type Range = (Int, Int)

-- RangeTuples are always sorted based on the end of the range 
data RangeVal = RangeTop
  | RangeBottom 
  | RangeTuple [Range]
  deriving (Eq, Show)

type Error = String

type Env = [(VName, RangeVal)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> RangeVal -> Env -> Env
envExtend v val env = (v, val) : env

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

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env ->
  case m1 env of
    Left _ -> m2 env
    Right x -> Right x

tupleJoin :: [Range] -> [Range] -> [Range]
tupleJoin [] [] = []
tupleJoin [] lst = lst
tupleJoin lst [] = lst
tupleJoin lst1@((int1, int2):rest1) lst2@((int3, int4):rest2)
  | int2 >= int3 && int4 >= int1 = (min int1 int3, max int2 int4) : tupleJoin rest1 rest2 -- Overlap of intervals 
  | int2 < int3 = (int1, int2) : tupleJoin rest1 lst2
  | otherwise = (int3, int4) : tupleJoin lst1 rest2

rangesJoin :: RangeVal -> RangeVal -> RangeVal
rangesJoin (RangeTuple lst1) (RangeTuple lst2) = RangeTuple $ tupleJoin lst1 lst2
-- rangesJoin (int1, int2) (RangeTuple lst@((int3, int4):rest2))
--   | int2 >= int3 && int4 >= int1 = RangeTuple $ Range (min int1 int3) (max int2 int4) : rest2 -- Overlap of intervals 
--   | int2 < int3 = rangesJoin (Range int1 int2) (RangeTuple rest2)
--   | otherwise = RangeTuple $ Range int3 int4 : lst
-- rangesJoin (int1, int2) (int3, int4)
--   | int2 >= int3 && int4 >= int1 = Range (min int1 int3) (max int2 int4) -- Overlap of intervals 
--   | int2 < int3 = RangeTuple [Range int1 int2, Range int3 int4]
--   | otherwise = RangeTuple [Range int3 int4, Range int1 int2]
rangesJoin any RangeBottom = any
rangesJoin RangeBottom any = any
rangesJoin _ RangeTop = RangeTop
rangesJoin RangeTop _ = RangeTop
-- rangesJoin any (int1, int2) = rangesJoin (int1, int2) any 

tupleIntersect :: [Range] -> [Range] -> [Range]
tupleIntersect [] [] = []
tupleIntersect [] lst = []
tupleIntersect lst [] = []
tupleIntersect lst1@((int1, int2):rest1) lst2@((int3, int4):rest2)
  | int2 >= int3 && int4 >= int1 = (max int1 int3, min int2 int4) : tupleIntersect rest1 rest2 -- Overlap of intervals 
  | int2 < int3 = tupleIntersect rest1 lst2
  | otherwise = tupleIntersect lst1 rest2

rangesIntersect :: RangeVal -> RangeVal -> RangeVal
rangesIntersect (RangeTuple lst1) (RangeTuple lst2) = 
  let lst = tupleIntersect lst1 lst2
  in
    if lst == []
      then RangeBottom
      else RangeTuple lst 
-- rangesIntersect (int1, int2) (RangeTuple lst@((int3, int4):rest2))
--   | int2 >= int3 && int4 >= int1 = RangeTuple $ Range (min int1 int3) (max int2 int4) : rest2 -- Overlap of intervals 
--   | int2 < int3 = rangesIntersect (Range int1 int2) (RangeTuple rest2)
--   | otherwise = RangeTuple $ Range int3 int4 : lst
-- rangesIntersect (int1, int2) (int3, int4)
--   | int2 >= int3 && int4 >= int1 = Range (min int1 int3) (max int2 int4) -- Overlap of intervals 
--   | int2 < int3 = RangeTuple [Range int1 int2, Range int3 int4]
--   | otherwise = RangeTuple [Range int3 int4, Range int1 int2]
rangesIntersect _ RangeBottom = RangeBottom
rangesIntersect RangeBottom _ = RangeBottom
rangesIntersect any RangeTop = any
rangesIntersect RangeTop any = any
-- rangesIntersect any (int1, int2) = rangesIntersect (int1, int2) any 

envIntersect :: Env -> Env -> Env
envIntersect [] [] = []
envIntersect env [] = env
envIntersect [] env = env
envIntersect ((name1, range1):rest1) ((name2, range2):rest2)
  | name1 == name2 = (name1, rangesIntersect range1 range2) : envIntersect rest1 rest2
  | name1 < name2 = (name1, range1) : envIntersect rest1 ((name2, range2):rest2)
  | otherwise = (name2, range2) : envIntersect ((name1, range1):rest1) rest2 

freeVNames :: Exp -> Env
freeVNames (CstInt _) = envEmpty
freeVNames (CstBool _) = envEmpty
freeVNames (Add e1 e2) = freeVNames e1 `envIntersect` freeVNames e2  
freeVNames (Sub e1 e2) = freeVNames e1 `envIntersect` freeVNames e2  
freeVNames (Mul e1 e2) = freeVNames e1 `envIntersect` freeVNames e2  
freeVNames (Eql e1 e2) = freeVNames e1 `envIntersect` freeVNames e2  
freeVNames (If e1 e2 e3) = freeVNames e1 `envIntersect` freeVNames e2 `envIntersect` freeVNames e3 
freeVNames (Tuple exps) = foldl (\acc elm -> acc `envIntersect` freeVNames elm) envEmpty exps
freeVNames (Project e _) = freeVNames e
freeVNames (Var name) = [(name, RangeTop)]
freeVNames (Lambda name e) = [(name, RangeTop)] `envIntersect` freeVNames e
freeVNames (Apply e1 e2) = freeVNames e1 `envIntersect` freeVNames e2  
freeVNames (ForLoop (name1, e1) (name2, e2) e3) = [(name1, RangeTop)] `envIntersect` [(name2, RangeTop)] `envIntersect` freeVNames e1 `envIntersect` freeVNames e2 `envIntersect` freeVNames e3 

binopRange :: (Int -> Int -> Int) -> RangeVal -> RangeVal -> RangeVal
binopRange _ RangeBottom _ = RangeBottom
binopRange _ _ RangeBottom = RangeBottom
binopRange _ RangeTop _ = RangeTop
binopRange _ _ RangeTop = RangeTop 
binopRange _ (RangeTuple []) a = RangeTuple []
binopRange _ a (RangeTuple []) = RangeTuple []
binopRange op lst1@(RangeTuple ((int1, int2):rest1)) lst2@(RangeTuple ((int3, int4):rest2)) =
  let (a,b) = (op int1 int3, op int2 int4)
  in rangesJoin (RangeTuple [(min a b, max a b)]) (rangesJoin (binopRange op lst1 (RangeTuple rest2)) (binopRange op (RangeTuple rest1) lst2))

ranges :: Exp -> EvalM RangeVal
ranges (CstInt val) = pure $ RangeTuple [(val, val)]
ranges (CstBool val) = pure RangeTop
ranges (Add e1 e2) = do
  v1 <- ranges e1
  v2 <- ranges e2
  pure $ binopRange (+) v1 v2 
ranges (Sub e1 e2) = do
  v1 <- ranges e1
  v2 <- ranges e2
  pure $ binopRange (-) v1 v2 
-- ranges (Mul e1 e2) = do
--   v1 <- ranges e1
--   v2 <- ranges e2
--   pure $ binopRange (*) v1 v2 
ranges (Eql e1 e2) = pure RangeTop
  -- do
  -- v1 <- ranges e1
  -- v2 <- ranges e2
  -- let v3 = v1 `rangesIntersect` v2
  -- in
  --   if v3 == RangeBottom
  --     then RangeTop
  --     else v3
  -- pure $ v1 `rangesJoin` v2
ranges (If e1 e2 e3) = do
  v1 <- ranges e1
  v2 <- ranges e2
  v3 <- ranges e3
  pure $ v2 `rangesJoin` v3 
ranges (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
-- ranges (Tuple exps) = 
-- ranges (Project e i) = do 
--   v1 <- ranges e
--   case v1 of
ranges (Apply (Lambda name e1) e2) = do
  v2 <- ranges e2
  env <- askEnv
  localEnv (const $ [(name, v2)] `envIntersect` env) $ ranges e1
ranges (Apply e1 e2) = ranges e1
ranges (Lambda name e) = pure RangeTop
-- ranges (ForLoop (name1, e1) (name2, e2) body) = do

