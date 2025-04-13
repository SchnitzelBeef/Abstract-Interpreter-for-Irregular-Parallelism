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
-- I.e. RangeTuple [Range 1 4, Range 100 200] is valid, but RangeTuple [Range 1, 4, Range -1, 0] isn't
data RangeVal = RangeTop
  | RangeBottom
  | Ranges [Range]
  | RangeTuple [RangeVal]
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

{- Joins two ranges, forming a lattice, i.e. ranges [Range 1 10, Range 5 12] becomes [Range 1 12]. Quite conservative -}
rangesJoin :: RangeVal -> RangeVal -> RangeVal
rangesJoin (RangeTuple tpl1) (RangeTuple tpl2) =
  if length tpl1 == length tpl2
    then RangeTuple $ zipWith rangesJoin tpl1 tpl2
    else RangeTop
rangesJoin (Ranges lst1) (Ranges lst2) = Ranges $ tupleJoin lst1 lst2
  where tupleJoin [] lst = lst
        tupleJoin lst [] = lst
        tupleJoin lst1@((int1, int2):rest1) lst2@((int3, int4):rest2)
          | int2 + 1 == int3 || int4 + 1 == int1 = (min int1 int3, max int2 int4) : tupleJoin rest1 rest2 -- Intervals next to each other 
          | int2 >= int3 && int4 >= int1 = (min int1 int3, max int2 int4) : tupleJoin rest1 rest2 -- Overlap of intervals 
          | int2 < int3 = (int1, int2) : tupleJoin rest1 lst2
          | otherwise = (int3, int4) : tupleJoin lst1 rest2
rangesJoin a RangeBottom = a
rangesJoin RangeBottom a = a
rangesJoin _ _ = RangeTop

{- Intersects two ranges, only used in intersecting environments, i.e. ranges [(1, 10), (5, 12)] becomes [(5, 10)] -}
rangesIntersect :: RangeVal -> RangeVal -> RangeVal
rangesIntersect (RangeTuple tpl1) (RangeTuple tpl2) =
  if length tpl1 == length tpl2
    then RangeTuple $ zipWith rangesIntersect tpl1 tpl2
    else RangeBottom
rangesIntersect (Ranges lst1) (Ranges lst2) = Ranges $ tupleIntersect lst1 lst2
  where tupleIntersect [] _ = []
        tupleIntersect _ [] = []
        tupleIntersect lst1@((int1, int2):rest1) lst2@((int3, int4):rest2)
          | int2 >= int3 && int4 >= int1 = (max int1 int3, min int2 int4) : tupleIntersect rest1 rest2 -- Overlap of intervals 
          | int2 < int3 = tupleIntersect rest1 lst2
          | otherwise = tupleIntersect lst1 rest2
rangesIntersect a RangeTop = a
rangesIntersect RangeTop a = a

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
freeVNames (Tuple exps) = foldl (\acc elm -> acc `envIntersect` freeVNames elm) envEmpty exps 
freeVNames (Project e _) = freeVNames e
freeVNames (Var name) = [(name, RangeTop)]
freeVNames (Lambda name e) = [(name, RangeTop)] `envIntersect` freeVNames e
freeVNames (Apply e1 e2) = freeVNames e1 `envIntersect` freeVNames e2
freeVNames (ForLoop (name1, e1) (name2, e2) e3) = [(name1, RangeTop)] `envIntersect` [(name2, RangeTop)] `envIntersect` freeVNames e1 `envIntersect` freeVNames e2 `envIntersect` freeVNames e3

{- Applies simple binary operator to a RangeVal, where it is ensured that the result is between the minimum and maximum of the intervals-}
simpleBinopRange :: (Int -> Int -> Int) -> RangeVal -> RangeVal -> RangeVal
simpleBinopRange _ RangeTop _ = RangeTop
simpleBinopRange _ _ RangeTop = RangeTop
simpleBinopRange op (RangeTuple tpl1) (RangeTuple tpl2) =
  if length tpl1 == length tpl2
    then RangeTuple $ zipWith (simpleBinopRange op) tpl1 tpl2
    else RangeTop
simpleBinopRange op lst1@(Ranges ((int1, int2):rest1)) lst2@(Ranges ((int3, int4):rest2)) =
  let lst = [op int1 int3, op int1 int4, op int2 int3, op int2 int4] -- Only works for the very-most simple of arithmetic
  in rangesJoin (Ranges [(minimum lst, maximum lst)]) (rangesJoin (simpleBinopRange op lst1 (Ranges rest2)) (simpleBinopRange op (Ranges rest1) lst2))
simpleBinopRange _ _ _ = RangeBottom

-- conditionalRanges :: Exp -> EvalM Env
-- conditionalRanges (Eql e1 e2) = do
--   v1 <- conditionalRanges e1
--   v2 <- conditionalRanges e2
--   case v

--   v4 <- localEnv (const $ [(name1, acc), (name2, RangeTuple [(j, j)])] `envIntersect` env) $ ranges body
--   v2 <- ranges e2
--   pure $ v1 `rangesIntersect` v2 
-- conditionalRanges (Var v) = v
-- conditionalRanges (Lt e1 e2) = do
--   v1 <- ranges e1
--   v2 <- ranges e2
--   case v2 of
--     RangeTuple ranges' -> snd $ last ranges
--   pure $ v1 `rangesLessThan` v1
-- conditionalRanges = ranges

{- Creates an abstract interpretation of the current expression and returns the expression can take-}
ranges :: Exp -> EvalM RangeVal
ranges (CstInt val) = pure $ Ranges [(val, val)]
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
ranges (Eql _ _) = pure RangeTop
ranges (If _ e2 e3) = do -- Currently just ignores e1, which is a point for improvement
  v2 <- ranges e2
  v3 <- ranges e3
  pure $ v2 `rangesJoin` v3
ranges (Tuple exps) =
 if null exps
  then do pure $ RangeTuple []
  else do 
    v1 <- ranges $ head exps
    v2 <- ranges $ Tuple $ drop 1 exps 
    case (v1, v2) of
      (range, RangeTuple []) -> pure $ RangeTuple [range]
      (range, RangeTuple tpl) -> pure $ RangeTuple (range : tpl)
ranges (Project e i) = do
  v1 <- ranges e
  case v1 of
    RangeTuple tpl -> pure $ tpl !! i
    _ -> pure v1 -- Could throw error instead
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
ranges (Lambda _ _) = pure RangeTop
ranges (ForLoop (name1, e1) (name2, e2) body) = do
  v1 <- ranges e1
  v2 <- ranges e2
  loop v1 (0, v2)
  where loop acc (j, tpl@(Ranges ((n, m):_))) = do
          env <- askEnv
          v4 <- localEnv (const $ [(name1, acc), (name2, Ranges [(j, j)])] `envIntersect` env) $ ranges body
          case v4 of
            Ranges _ | j < n -> loop v4 (j+1, tpl)
            Ranges _ | j >= m -> pure acc
            Ranges _ -> do
              v5 <- loop v4 (j+1, tpl)
              pure $ v4 `rangesJoin` v5
            r -> pure r
        loop _ _ = pure RangeTop -- Conservative
