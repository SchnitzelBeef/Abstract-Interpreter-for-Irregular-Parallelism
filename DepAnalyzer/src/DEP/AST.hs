module DEP.AST
  ( VName
  , Exp (..)
  )
where

type VName = String

{- The futhark grammar -}
data Exp
  = Def 
  deriving (Eq, Ord, Show)


{-
def arithmetic ((x: i32): i32) : i32 =
  x + x

entry main ((n: i32): i32) : i32 =
  arithmetic n
-}