module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
-- import APL.Eval (Val (..))
import APL.Absin (ranges, RangeVal (..), runRanges, Error, Env, envIntersect, freeVNames)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

ranges' :: Env -> Exp -> Either Error RangeVal
ranges' env e = runRanges (env `envIntersect` freeVNames e) $ ranges e

tests :: TestTree
tests =
  testGroup
    "Abstract"
    [ testCase "Add" $
        ranges' [] (Add (CstInt 3) (Add (CstInt 1) (CstInt 10)))
          @?= Right (RangeTuple [(14, 14)]),
      testCase "Add with environment" $
        ranges' [("x", RangeTuple [(1, 5)])] (Add (CstInt 3) (Var "x"))
          @?= Right (RangeTuple [(4, 8)]),
      testCase "Add with free variables" $
        ranges' [] (Add (CstInt 3) (Var "x"))
          @?= Right RangeTop,
      testCase "Add with environment and free variables" $
        ranges' [("x", RangeTuple [(1, 5)])] (Add (Var "y") (Var "x"))
          @?= Right RangeTop,
      testCase "If" $
        ranges' [("x", RangeTuple [(1, 5)])] (If (Var "y") (Add (Var "x") (Var "x")) (Sub (Var "x") (CstInt 10)))
          @?= Right (RangeTuple [(-9, -5), (2, 10)]),
      testCase "Eql possible" $
        ranges' [("x", RangeTuple [(1, 5)])] (Eql (Var "x") (CstInt 3))
          @?= Right RangeTop,
      testCase "Eql not possible" $
        ranges' [("x", RangeTuple [(1, 5)])] (Eql (Var "x") (CstInt 10))
          @?= Right RangeTop,
      testCase "Apply lambda" $
        ranges' [] (Apply (Lambda ("x") (Add (Var "x") (CstInt 1))) (CstInt 5))
          @?= Right (RangeTuple [(6, 6)]),
      testCase "Apply lambda on predefined env" $
        ranges' [("y", RangeTuple [(0, 1), (4, 5)])] (Apply (Lambda ("x") (Add (Var "x") (CstInt 1))) (Var "y"))
          @?= Right (RangeTuple [(1, 2), (5, 6)])        
    ]
