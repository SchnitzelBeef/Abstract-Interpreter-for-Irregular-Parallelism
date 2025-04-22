## Range analyzer 
Abstract interpreter of the (slightly reduced) APL language provided by Troels Henriksen

The interpreter makes sound - yet nowhere tight - abstractions on a given expression of the language.
This solely includes integer ranges. If the expression returns a boolean, a value "RangeBottom" is returned instead to denote the lack of information. This is also true for the return of free variables in some circumstances, where we can't deduce that the type is an integer. For example, adding a free variable "x" to a constant in the test:

        ranges' [] (Add (CstInt 3) (Var "x"))

gives the result "RangeTop", while the test with a pre-defined environment:

        ranges' [("x", Ranges [(1, 5)])] (Add (CstInt 3) (Var "x"))

returns "Ranges [(4, 8)]". BUT if we simply interpret the expression:

        (Var "x")

we would get "RangeBottom" instead.

The interpreter forms lattices with resp. RangeTop and RangeBottom at the top and bottom of the lattice.
Testing is done in "Eval_tests.hs". Interpreting a more complex example:

        ranges' [("y", Ranges [(0, 1), (4, 5)])] (Apply (Lambda ("x") (Add (Var "x") (CstInt 1))) (Var "y"))

yields the result "Ranges [(1, 2), (5, 6)]" as expected.

The tuples are always sorted based on the last element to retain the logic of the interpreter.

We separate between actual tuples - RangeTuple - and ranges - Range - in the data-type "RangeVal" even though they are quite similar. To clarify:
-> "Ranges" are the ranges a single variable may take, e.g. in the ranges [(1, 3), (8, 13)], whilst
-> "RangeTuples" are actual tuple values of other RangeVal's, i.e. [][]
-> These data-types *cannot* be joined without resulting in a RangeTop, since they are fundamentally different, but two RangeTuples consisting of a broad range of (differently sized) ranges can be merged. For example, joining the following in a rangesJoin call: 

        > rangesJoin
                (RangeTuple [Ranges [(1, 2), (4, 5)], RangeTuple [Ranges [(1, 10)], RangeTop]])
                (RangeTuple [Ranges [(5, 10)], RangeTuple [Ranges [(1, 4), (7, 14)], RangeBottom])
        =  RangeTuple [Ranges [(1, 2), (4, 10)], RangeTuple [Ranges [(1, 14)], RangeTop]]

Lastly, I note that all variables should be classified by unique identifiers even though they are not in the same scope - otherwise the behaviour is undefined.

The code is based on the code given in the AP-course (specifically week 2 and 3) - Thank you!
