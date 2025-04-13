module APL.AST
  ( VName
  , Exp (..)
  )
where

type VName = String

{- The APL grammar provided by Troels Henriksen (without explicit tuples) -}
data Exp
  = CstInt Int
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Tuple [Exp]
  | Project Exp Int
  | Var VName 
  | Lambda VName Exp
  | Apply Exp Exp
  | ForLoop (VName, Exp) (VName, Exp) Exp
  deriving (Eq, Ord, Show)
