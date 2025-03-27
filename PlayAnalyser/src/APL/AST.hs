module APL.AST
  ( VName
  , Exp (..)
  )
where

type VName = String

{- The APL grammar provided by Troels Henriksen extended with two new concepts -}
data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  -- Grammar constructs below are added for a more meaningful analysis (in theory... maybe):
  | RandomInt Exp Exp -- Creates a random int in the range of the two expressions (inclusive)
  | Loop VName Exp VName Exp Exp
  {- ^^ From https://sigkill.dk/programs/absin.html
    "Loop (p, pe) (i, ie) body"
    Equiv to:
      p = pe
      for i < ie:
        p = body
      return p
  -}
  deriving (Eq, Show)
