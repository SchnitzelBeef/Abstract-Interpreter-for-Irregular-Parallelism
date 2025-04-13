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
          @?= Right RangeTop,
      testCase "Add" $
        ranges' [] (Add (CstInt 3) (Add (CstInt 1) (CstInt 10)))
          @?= Right (Ranges [(14, 14)]),
      testCase "Add with environment" $
        ranges' [("x", Ranges [(1, 5)])] (Add (CstInt 3) (Var "x"))
          @?= Right (Ranges [(4, 8)]),
      testCase "Add with free variables" $
        ranges' [] (Add (CstInt 3) (Var "x"))
          @?= Right RangeTop,
      testCase "Add with environment and free variables" $
        ranges' [("x", Ranges [(1, 5)])] (Add (Var "y") (Var "x"))
          @?= Right RangeTop,
      testCase "Gen. arithmetic 1" $
        ranges' [("x", Ranges [(-5, -2)]), ("y", Ranges [(1, 2), (10, 20)])]
                  (Sub (Mul (Var "x") (Var "y")) (Var "y"))
                  @?= Right (Ranges [(-120, -3)]),
      testCase "Gen. arithmetic 2" $
        ranges' [("x", Ranges [(-5, -2)]), ("y", Ranges [(1, 2), (10, 20)])]
                  (Add (Mul (Var "x") (Var "y")) (Var "x"))
                  @?= Right (Ranges [(-105, -22), (-15, -4)]),
      testCase "If" $
        ranges' [("x", Ranges [(1, 5)])] (If (Var "y") (Add (Var "x") (Var "x")) (Sub (Var "x") (CstInt 10)))
          @?= Right (Ranges [(-9, -5), (2, 10)]),
      testCase "Eql possible" $
        ranges' [("x", Ranges [(1, 5)])] (Eql (Var "x") (CstInt 3))
          @?= Right RangeTop,
      testCase "Eql not possible" $
        ranges' [("x", Ranges [(1, 5)])] (Eql (Var "x") (CstInt 10))
          @?= Right RangeTop,
      testCase "Apply lambda" $
        ranges' [] (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstInt 5))
          @?= Right (Ranges [(6, 6)]),
      testCase "Apply lambda on predefined env" $
        ranges' [("y", Ranges [(0, 1), (4, 5)])] (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (Var "y"))
          @?= Right (Ranges [(1, 2), (5, 6)]),        
      testCase "Union close ranges" $
        ranges' [] (If (Var "x") (CstInt 1) (CstInt 2))
          @?= Right (Ranges [(1, 2)]),
      testCase "Lambda on free variables" $
        ranges' [] (Apply (Lambda "x" (Add (Var "y") (Var "x"))) (CstInt 1))
          @?= Right RangeTop,
      testCase "Normal loop" $
        ranges' [] (ForLoop ("acc", CstInt 0) ("i", CstInt 10)
                      (Add (Var "acc") (Add (Var "i") (CstInt 1))))
          @?= Right (Ranges [(55, 55)]),
      testCase "Loop with range in termination" $
        ranges' [("x", Ranges [(5, 10)])] (ForLoop ("acc", CstInt 0) ("i", Var "x")
                      (Add (Var "acc") (Add (Var "i") (CstInt 1))))
          @?= Right (Ranges [(21,21),(28,28),(36,36),(45,45),(55,55)]),
      testCase "Loop with range in accumulator" $
        ranges' [("x", Ranges [(5, 10)])] (ForLoop ("acc", Var "x") ("i", CstInt 10)
                      (Add (Var "acc") (Add (Var "i") (CstInt 1))))
          @?= Right (Ranges [(60, 65)]),
      testCase "Loop with range-bottom in termination" $
        ranges' [] (ForLoop ("acc", CstInt 0) ("i", Var "x")
                      (Add (Var "acc") (Add (Var "i") (CstInt 1))))
          @?= Right RangeTop,
      testCase "Loop with range-bottom in accumulator" $
        ranges' [] (ForLoop ("acc", Var "x") ("i", CstInt 10)
                      (Add (Var "acc") (Add (Var "i") (CstInt 1))))
          @?= Right RangeTop,
      testCase "If equals with env" $
        ranges' [("x", Ranges [(-1, 2)])] (If (Eql (Var "x") (CstInt 5)) (Var "x") (CstInt 10))
          @?= Right (Ranges [(-1, 2), (10, 10)]),
      -- ^^ Returns both possibilities even though the first path can never be entered due to approximations
      testCase "If equals with free vars" $
        ranges' [] (If (Eql (Var "x") (CstInt 5)) (Var "x") (CstInt 10))
          @?= Right RangeTop, -- Does not take into account the condition on x
      testCase "Tuples without equal dimensions" $
        ranges' [] (If (Var "x") (Tuple [CstInt 1, CstInt 2]) (Var "x"))
          @?= Right RangeTop,
      testCase "Tuples with equal dimensions" $
        ranges' [] (If (Var "x") (Tuple [CstInt 1, CstInt 2]) (Tuple [CstInt 2, CstInt 4]))
          @?= Right (RangeTuple [Ranges [(1, 2)], Ranges [(2, 2), (4, 4)]]),
      testCase "Nested tuples" $
        ranges' [] (If (Var "x") (Tuple [CstInt 1, Tuple [CstInt 5, Var "x"]]) (Tuple [CstInt 2, CstInt 4]))
          @?= Right (RangeTuple [Ranges [(1, 2)], RangeTop])
    ]
