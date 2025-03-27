module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..))
import APL.Absin (interpret)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

{-
Output is always sorted in reverse alphabetical order in terms of variables
The output represents an abstraction of the ranges a variable spans after it is evaluated.
The abstraction is not completely tight, but should be sound.
-}

tests :: TestTree
tests =
  testGroup
    "Abstract"
    [ testCase "Let" $
        interpret [] (Let "y" (CstInt 5) (Var "y"))  @?= [("y", ValIntRange 5 5)],
        --
      testCase "Random" $
        interpret [] (Let "y" (RandomInt (CstInt (-1)) (Add (CstInt 2) (CstInt 3))) (Var "y"))  @?= [("y", ValIntRange (-1) 5)],
        --
      testCase "Nested lets" $
        interpret [] (Let "y" (RandomInt (CstInt 1) (CstInt 5)) 
                        (Let "x" (RandomInt (CstInt 1) (CstInt 10)) 
                          (Sub (Var "y") (Var "x")))) @?= [("y", ValIntRange 1 5), ("x", ValIntRange 1 10)],
        --
      testCase "Add Randoms" $
        interpret [] (Let "y" (RandomInt (CstInt 1) (CstInt 5)) 
                        (Let "x" (RandomInt (CstInt 1) (CstInt 10)) 
                          (Let "z" (Add (Var "x") (Var "y"))
                            (Var "z")))) @?= [("z", ValIntRange 2 15), ("y", ValIntRange 1 5), ("x", ValIntRange 1 10)],
      --
      testCase "Div with only positive bases" $
        interpret [] (Let "y" (RandomInt (CstInt 2) (CstInt 5)) 
                        (Let "x" (Div (CstInt 10) (Var "y")) 
                          (Var "x"))) @?= [("y", ValIntRange 2 5), ("x", ValIntRange 2 5)],
      --
      testCase "Div with potential negative base" $
        interpret [] (Let "y" (RandomInt (CstInt (-2)) (CstInt 5)) 
                        (Let "x" (Div (CstInt 10) (Var "y")) 
                          (Var "x"))) @?= [("y", ValIntRange (-2) 5), ("x", None)],
      --
      testCase "Start with arithmetic" $
        interpret [] (Sub (CstInt 10)
                        (Let "y" (RandomInt (CstInt 2) (CstInt 5))
                          (Var "y"))) @?= [("y",ValIntRange 2 5)],
      --
      testCase "If-cond can evaluate to true and false" $
        interpret [] (Let "y"
                        (If (Eql (RandomInt (CstInt 0) (CstInt 4)) (CstInt 0))
                          (CstInt 1)    -- "then" branch
                          (CstInt 4))   -- "else" branch
                          (CstInt 0))  @?= [("y", ValIntRange 1 4)],
      --
      testCase "If-cond can only evaluate to true" $
        interpret [] (Let "y"
                        (If (CstBool True)
                          (CstInt 1)    -- "then" branch
                          (CstInt 4))   -- "else" branch
                          (CstInt 0))  @?= [("y", ValIntRange 1 1)],
      --
      testCase "Loop" $ -- Example from https://sigkill.dk/programs/absin.html
        interpret [] (Let "y" (Loop "acc" (CstInt 1) "i" (CstInt 10)
                        (Mul (Var "acc") (Add (Var "i") (CstInt 1))))
                        (Var "y")) @?= [("y", ValIntRange 3628800 3628800)],
      --
      testCase "Loop on range" $
        interpret [] (Let "y" (RandomInt (CstInt 2) (CstInt 4))
                        (Let "x" (Loop "acc" (CstInt 0) "i" (Var "y")
                          (Sub (Var "acc") (Var "i")))
                          (Sub (Var "y") (Var "x")))) @?= [("y", ValIntRange 2 4), ("x", ValIntRange (-6) (-1))],
      --
      testCase "Both boolean cases" $
        interpret [] (Let "y" (Eql (RandomInt (CstInt 0) (CstInt 1)) (CstInt 0))
                        (Var "y")) @?= [("y", ValBoolBoth)]
    ]
