module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, abstractFixpoint)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Abstract"
    [ testCase "Let" $
        abstractFixpoint (Let "x" (CstInt 5) (Add (Var "x") (CstInt 2)))  @?= [("x", [])]
    ]
