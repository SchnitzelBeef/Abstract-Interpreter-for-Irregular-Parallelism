## Range analyzer 
Abstract interpreter of the (slightly reduced) APL language provided by Troels Henriksen

The interpreter makes sound - yet nowhere tight - abstractions on a given expression of the language.
This solely includes integer ranges. If the expression returns a boolean, a value "RangeBottom" is returned instead to denote the lack of information. This is also true for the return of free variables in some circumstances, where we can't deduce that the type is an integer. For example, adding a free variable "x" to a constant in the test:

        ranges' [] (Add (CstInt 3) (Var "x"))

gives the result "RangeTop", while the test with a pre-defined environment:

        ranges' [("x", RangeTuple [(1, 5)])] (Add (CstInt 3) (Var "x"))

returns "RangeTuple [(4, 8)]". BUT if we simply interpret the expression:

        (Var "x")

we would get "RangeBottom" instead.

The interpreter forms lattices with resp. RangeTop and RangeBottom at the top and bottom of the lattice.
Testing is done in "Eval_tests.hs". Interpreting a more complex example:

        ranges' [("y", RangeTuple [(0, 1), (4, 5)])] (Apply (Lambda ("x") (Add (Var "x") (CstInt 1))) (Var "y"))

yields the result "RangeTuple [(1, 2), (5, 6)]" as expected.

The tuples are always sorted based on the last element to retain the logic of the interpreter.
Lastly, I note that all variables should be classified by unique identifiers even though they are not in the same scope - otherwise the behaviour is undefined.

The code is based on the code given in the AP-course (specifically week 2 and 3) - Thank you!
