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

type Range = (Int, Int)

-- An invariant to the RangeTuple, is that they are always sorted based on the end of the range
-- I.e. RangeTuple [(1, 4), (100, 200)] is valid, but RangeTuple [(1, 4), (-1, 0)] isn't
data RangeVal = RangeTop
  | RangeBottom 
  | RangeTuple [Range]
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

{- Joins two tuples, i.e. ranges [(1, 10), (5, 12)] becomes [(1, 12)], should only be called from rangesJoin-}
tupleJoin :: [Range] -> [Range] -> [Range]
tupleJoin [] lst = lst
tupleJoin lst [] = lst
tupleJoin lst1@((int1, int2):rest1) lst2@((int3, int4):rest2)
  | int2 + 1 == int3 || int4 + 1 == int1 = (min int1 int3, max int2 int4) : tupleJoin rest1 rest2 -- Intervals next to each other 
  | int2 >= int3 && int4 >= int1 = (min int1 int3, max int2 int4) : tupleJoin rest1 rest2 -- Overlap of intervals 
  | int2 < int3 = (int1, int2) : tupleJoin rest1 lst2
  | otherwise = (int3, int4) : tupleJoin lst1 rest2

{- Joins two ranges, forming a lattice-}
rangesJoin :: RangeVal -> RangeVal -> RangeVal
rangesJoin (RangeTuple lst1) (RangeTuple lst2) = 
  let lst = tupleJoin lst1 lst2
  in
    if lst == []
      then RangeBottom 
      else RangeTuple $ lst
rangesJoin _ RangeTop = RangeTop
rangesJoin RangeTop _ = RangeTop
rangesJoin r RangeBottom = r
rangesJoin RangeBottom r = r

{- Intersects two tuples, i.e. ranges [(1, 10), (5, 12)] becomes [(5, 10)], should only be called from rangesIntersect-}
tupleIntersect :: [Range] -> [Range] -> [Range]
tupleIntersect [] _ = []
tupleIntersect _ [] = []
tupleIntersect lst1@((int1, int2):rest1) lst2@((int3, int4):rest2)
  | int2 >= int3 && int4 >= int1 = (max int1 int3, min int2 int4) : tupleIntersect rest1 rest2 -- Overlap of intervals 
  | int2 < int3 = tupleIntersect rest1 lst2
  | otherwise = tupleIntersect lst1 rest2

{- Intersects two ranges, only used in intersecting environments -}
rangesIntersect :: RangeVal -> RangeVal -> RangeVal
rangesIntersect (RangeTuple lst1) (RangeTuple lst2) = 
  let lst = tupleIntersect lst1 lst2
  in
    if lst == []
      then RangeBottom
      else RangeTuple lst 
rangesIntersect r RangeBottom = r
rangesIntersect RangeBottom r = r
rangesIntersect _ RangeTop = RangeTop
rangesIntersect RangeTop _ = RangeTop

{- Intersects two environments, which is used in lambda definitions -}
envIntersect :: Env -> Env -> Env
envIntersect env [] = env
envIntersect [] env = env
envIntersect ((name1, range1):rest1) ((name2, range2):rest2)
  | name1 == name2 = (name1, rangesIntersect range1 range2) : envIntersect rest1 rest2
  | name1 < name2 = (name1, range1) : envIntersect rest1 ((name2, range2):rest2)
  | otherwise = (name2, range2) : envIntersect ((name1, range1):rest1) rest2 

{- Identifies all free variables in the expression -}
freeVNames :: Exp -> Env
freeVNames (CstInt _) = envEmpty
freeVNames (CstBool _) = envEmpty
freeVNames (Add e1 e2) = freeVNames e1 `envIntersect` freeVNames e2  
freeVNames (Sub e1 e2) = freeVNames e1 `envIntersect` freeVNames e2  
freeVNames (Mul e1 e2) = freeVNames e1 `envIntersect` freeVNames e2  
freeVNames (Eql e1 e2) = freeVNames e1 `envIntersect` freeVNames e2  
freeVNames (If e1 e2 e3) = freeVNames e1 `envIntersect` freeVNames e2 `envIntersect` freeVNames e3 
freeVNames (Var name) = [(name, RangeBottom)]
freeVNames (Lambda name e) = [(name, RangeBottom)] `envIntersect` freeVNames e
freeVNames (Apply e1 e2) = freeVNames e1 `envIntersect` freeVNames e2  
freeVNames (ForLoop (name1, e1) (name2, e2) e3) = [(name1, RangeBottom)] `envIntersect` [(name2, RangeBottom)] `envIntersect` freeVNames e1 `envIntersect` freeVNames e2 `envIntersect` freeVNames e3 

{- Applies simple binary operator to a RangeVal, where it is ensured that the result is between the minimum and maximum of the intervals-}
simpleBinopRange :: (Int -> Int -> Int) -> RangeVal -> RangeVal -> RangeVal
simpleBinopRange _ RangeTop _ = RangeTop
simpleBinopRange _ _ RangeTop = RangeTop 
simpleBinopRange _ RangeBottom _ = RangeTop -- This logic might be a bit iffy
simpleBinopRange _ _ RangeBottom = RangeTop -- This logic might be a bit iffy
simpleBinopRange _ (RangeTuple []) _ = RangeTuple []
simpleBinopRange _ _ (RangeTuple []) = RangeTuple []
simpleBinopRange op lst1@(RangeTuple ((int1, int2):rest1)) lst2@(RangeTuple ((int3, int4):rest2)) =
  let lst = [op int1 int3, op int1 int4, op int2 int3, op int2 int4] -- Only works for the very-most simple of arithmetic
  in rangesJoin (RangeTuple [(minimum lst, maximum lst)]) (rangesJoin (simpleBinopRange op lst1 (RangeTuple rest2)) (simpleBinopRange op (RangeTuple rest1) lst2))

{- Creates an abstract interpretation of the current expression and returns the possible ranges the expression can take-}
ranges :: Exp -> EvalM RangeVal
ranges (CstInt val) = pure $ RangeTuple [(val, val)]
ranges (CstBool _) = pure RangeTop
ranges (Add e1 e2) = do
  v1 <- ranges e1
  v2 <- ranges e2
  pure $ simpleBinopRange (+) v1 v2 
ranges (Sub e1 e2) = do
  v1 <- ranges e1
  v2 <- ranges e2
  pure $ simpleBinopRange (-) v1 v2 
ranges (Mul e1 e2) = do
  v1 <- ranges e1
  v2 <- ranges e2
  pure $ simpleBinopRange (*) v1 v2 
ranges (Eql _ _) = pure RangeBottom -- Might be wrong to return RangeBottom, but we lack better grammar for booleans
ranges (If _ e2 e3) = do -- Currently just ignores e1, which is a point for improvement
  v2 <- ranges e2
  v3 <- ranges e3
  pure $ v2 `rangesJoin` v3 
ranges (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
ranges (Apply (Lambda name e1) e2) = do
  v2 <- ranges e2
  env <- askEnv
  localEnv (const $ [(name, v2)] `envIntersect` env) $ ranges e1
ranges (Apply e1 _) = ranges e1
ranges (Lambda _ _) = pure RangeBottom
ranges (ForLoop (name1, e1) (name2, e2) body) = do
  v1 <- ranges e1
  v2 <- ranges e2
  v3 <- loop v1 (0, v2)
  pure v3
  where loop acc (j, tpl@(RangeTuple ((n, m):_))) = do
          env <- askEnv
          v4 <- localEnv (const $ [(name1, acc), (name2, RangeTuple [(j, j)])] `envIntersect` env) $ ranges body
          case v4 of
            RangeTuple _ | j < n -> loop v4 (j+1, tpl)
            RangeTuple _ | j >= m -> pure acc
            RangeTuple _ -> do
              v5 <- loop v4 (j+1, tpl)
              pure $ v4 `rangesJoin` v5
            r -> pure r
        loop _ _ = pure RangeBottom -- This logic might be a bit shady
