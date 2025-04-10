module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Absin (ranges, RangeVal (..), runRanges, Error, Env, envIntersect, freeVNames)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

ranges' :: Env -> Exp -> Either Error RangeVal
ranges' env e = runRanges (env `envIntersect` freeVNames e) $ ranges e

tests :: TestTree
tests =
  testGroup
    "Abstract"
    [ testCase "Free variables" $
        ranges' [] (Var "x")
          @?= Right RangeBottom,
      testCase "Add" $
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
      testCase "Gen. arithmetic 1" $
        ranges' [("x", RangeTuple [(-5, -2)]), ("y", RangeTuple [(1, 2), (10, 20)])]
                  (Sub (Mul (Var "x") (Var "y")) (Var "y"))
                  @?= Right (RangeTuple [(-120, -3)]),
      testCase "Gen. arithmetic 2" $
        ranges' [("x", RangeTuple [(-5, -2)]), ("y", RangeTuple [(1, 2), (10, 20)])]
                  (Add (Mul (Var "x") (Var "y")) (Var "x"))
                  @?= Right (RangeTuple [(-105, -22), (-15, -4)]),
      testCase "If" $
        ranges' [("x", RangeTuple [(1, 5)])] (If (Var "y") (Add (Var "x") (Var "x")) (Sub (Var "x") (CstInt 10)))
          @?= Right (RangeTuple [(-9, -5), (2, 10)]),
      testCase "Eql possible" $
        ranges' [("x", RangeTuple [(1, 5)])] (Eql (Var "x") (CstInt 3))
          @?= Right RangeBottom,
      testCase "Eql not possible" $
        ranges' [("x", RangeTuple [(1, 5)])] (Eql (Var "x") (CstInt 10))
          @?= Right RangeBottom,
      testCase "Apply lambda" $
        ranges' [] (Apply (Lambda ("x") (Add (Var "x") (CstInt 1))) (CstInt 5))
          @?= Right (RangeTuple [(6, 6)]),
      testCase "Apply lambda on predefined env" $
        ranges' [("y", RangeTuple [(0, 1), (4, 5)])] (Apply (Lambda ("x") (Add (Var "x") (CstInt 1))) (Var "y"))
          @?= Right (RangeTuple [(1, 2), (5, 6)]),        
      testCase "Union close ranges" $
        ranges' [] (If (Var "x") (CstInt 1) (CstInt 2))
          @?= Right (RangeTuple [(1, 2)]),
      testCase "Lambda on free variables" $
        ranges' [] (Apply (Lambda ("x") (Add (Var "y") (Var "x"))) (CstInt 1))
          @?= Right RangeTop,
      testCase "Normal loop" $
        ranges' [] (ForLoop ("acc", CstInt 0) ("i", CstInt 10)
                      (Add (Var "acc") (Add (Var "i") (CstInt 1))))
          @?= Right (RangeTuple [(55, 55)]),
      testCase "Loop with range in termination" $
        ranges' [("x", RangeTuple [(5, 10)])] (ForLoop ("acc", CstInt 0) ("i", (Var "x"))
                      (Add (Var "acc") (Add (Var "i") (CstInt 1))))
          @?= Right (RangeTuple [(21,21),(28,28),(36,36),(45,45),(55,55)]),
      testCase "Loop with range in accumulator" $
        ranges' [("x", RangeTuple [(5, 10)])] (ForLoop ("acc", (Var "x")) ("i", (CstInt 10))
                      (Add (Var "acc") (Add (Var "i") (CstInt 1))))
          @?= Right (RangeTuple [(60, 65)]),
      testCase "Loop with range-bottom in termination" $
        ranges' [] (ForLoop ("acc", (CstInt 0)) ("i", (Var ("x")))
                      (Add (Var "acc") (Add (Var "i") (CstInt 1))))
          @?= Right RangeBottom,
      testCase "Loop with range-bottom in accumulator" $
        ranges' [] (ForLoop ("acc", (Var "x")) ("i", (CstInt 10))
                      (Add (Var "acc") (Add (Var "i") (CstInt 1))))
          @?= Right RangeTop,
      testCase "If equals with env" $
        ranges' [("x", RangeTuple [(-1, 2)])] (If (Eql (Var "x") (CstInt 5)) (Var "x") (CstInt 10))
          @?= Right (RangeTuple [(-1, 2), (10, 10)]),
      -- ^^ Returns both possibilities even though the first path can never be entered due to approximations
      testCase "If equals with free vars" $
        ranges' [] (If (Eql (Var "x") (CstInt 5)) (Var "x") (CstInt 10))
          @?= Right (RangeTuple [(10, 10)]) -- Does not return RangeTop, as "x" might be anything
    ]
