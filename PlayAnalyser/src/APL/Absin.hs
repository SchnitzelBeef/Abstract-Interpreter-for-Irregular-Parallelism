module APL.Absin
  ( Val (..),
    AEnv,
    interpret,
    eval'
  )
where

import APL.AST (Exp (..), VName)
import APL.Eval (Val (..), Env, eval, runEval)

type Error = String

{-
Types for the abstract interpreter.
Currently only forms forms a "single-file" lattice since "UnboundValues" only exists of a single value - that could be a range
This means that if a variable can take a value -2 and 2, it will be abstracted into all values in the range [-2, -1, ..., 2] 
This would be the next step of improvements to split this into two separate abstractions for the same identifier.
Concretely, we would start by changing UnboundValues into "type UnboundValues = [Val]""
-}
type UnboundValues = Val
type BoundValues = (VName, UnboundValues)
type AEnv = [BoundValues]

eval' :: Env -> Exp -> Either Error Val
eval' env = runEval env . eval


getName :: BoundValues -> VName
getName = fst

getValues :: BoundValues -> UnboundValues
getValues = snd

{- | Insert a VName-range binding into the abstract environment in correct place to retain property of being sorted  -}
aEnvInsert :: BoundValues -> AEnv -> AEnv
aEnvInsert x (y:ys)
  | getName x >= getName y = x : (y : ys)
  | getName x <=  getName y = y : aEnvInsert x ys
aEnvInsert x [] = [x]

{- | Binds name to value  -}
bind :: VName -> Val -> BoundValues
bind var val = (var, val)

{- | Union of abstract environments  -}
aEnvUnion :: AEnv -> AEnv -> AEnv
aEnvUnion [] [] = []
aEnvUnion [] ys = ys
aEnvUnion xs [] = xs
aEnvUnion (x:xs) (y:ys)
  | getName x == getName y = 
    case (getValues x, getValues y) of
      (ValIntRange int1 int2, ValIntRange int3 int4) -> (getName x, ValIntRange (min int1 int3) (max int2 int4)) : aEnvUnion xs ys
      (ValBoolBoth, ValBoolBoth) -> (getName x, ValBoolBoth) : aEnvUnion xs ys
      (ValBoolBoth, ValBool _) -> (getName x, ValBoolBoth) : aEnvUnion xs ys
      (ValBool _, ValBoolBoth) -> (getName x, ValBoolBoth) : aEnvUnion xs ys
      (ValBool x', ValBool y') -> if x' /= y'
        then (getName x, ValBoolBoth): aEnvUnion xs ys
        else (getName x, ValBool x'): aEnvUnion xs ys
      (_, _) -> (getName x, None) : aEnvUnion xs ys
      -- Should prob. throw error instead (mix of two different types in variables)
  | getName x > getName y = x : aEnvUnion xs (y : ys) 
  | getName x < getName y = y : aEnvUnion (x : xs) ys
  
{- | Interprets the ranges values can take with severe abstractions -}
interpret :: AEnv -> Exp -> AEnv
interpret aenv (CstInt _) = aenv
interpret aenv (CstBool _) = aenv
interpret aenv (Var _) = aenv
interpret aenv (Add e1 e2) = aEnvUnion (interpret aenv e1) (interpret aenv e2)
interpret aenv (Sub e1 e2) = aEnvUnion (interpret aenv e1) (interpret aenv e2)
interpret aenv (Mul e1 e2) = aEnvUnion (interpret aenv e1) (interpret aenv e2)
interpret aenv (Div e1 e2) = aEnvUnion (interpret aenv e1) (interpret aenv e2)
interpret aenv (Pow e1 e2) = aEnvUnion (interpret aenv e1) (interpret aenv e2)
interpret aenv (Eql e1 e2) = aEnvUnion (interpret aenv e1) (interpret aenv e2)
interpret aenv (If cond e1 e2) = aEnvUnion (interpret aenv cond) $ aEnvUnion (interpret aenv e1) (interpret aenv e2)
    -- ^^ Could evaluate "cond" to see if e1 or e2 (or both) should be entered
interpret aenv (Let var e1 e2) = 
  case eval' aenv e1 of
    Right val -> aEnvUnion (interpret aenv e1) (interpret (aEnvInsert (bind var val) aenv) e2)
    Left _ -> aEnvUnion (interpret aenv e1) (interpret (aEnvInsert (bind var None) aenv) e2)
    --  ^^ Should prob. throw error instead
interpret aenv (RandomInt e1 e2) = aEnvUnion (interpret aenv e1) (interpret aenv e2)
interpret aenv (Lambda _ body) = interpret aenv body
interpret aenv (Apply e1 e2) = aEnvUnion (interpret aenv e1) (interpret aenv e2)
interpret aenv (TryCatch e1 e2) = aEnvUnion (interpret aenv e1) (interpret aenv e2)
interpret aenv (Loop _ _ _ _ _) = aenv -- Does not consider inner loop ranges, could be added...
