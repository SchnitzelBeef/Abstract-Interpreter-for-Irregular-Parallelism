## Play analyzer 
Abstract interpreter of the (slightly extended) APL language provided by Troels Henriksen

The interpreter makes sound - yet nowhere tight - abstractions on the language.
This includes integer ranges and boolean values a variable may take.

Currently, the interpreter only forms forms a "single-file" lattice since the datatype "UnboundValues" (see Absin.hs) only exists of a single range at a time.
This means that if a variable can take a value -2 and 2, it will be abstracted into all values in the range [-2, -1, ..., 2], instead of the tighter [[-2, -2], [2, 2]]
This would be the next step of improvements to split this into two separate abstractions for the same identifier for a more meaningful analysis.

Since the language is functional and has immutable variables, the interpreter has an "easy" time evaluating the span of an identifier only once.

Testing is done in "Eval_tests.hs". Interpreting the following let bindings:

(Let "y" (RandomInt (CstInt 1) (CstInt 5)) 
    (Let "x" (RandomInt (CstInt 1) (CstInt 10)) 
        (Let "z" (Add (Var "x") (Var "y"))
            (Var "z")))) 

... yields the result:            

    [("z", ValIntRange 2 15), ("y", ValIntRange 1 5), ("x", ValIntRange 1 10)]

The result is always represented in reverse alphabetical order and represents an abstraction of the ranges a variable spans after it is evaluated fully.
Note that the last line of the code "(Var "z")" does absolutely nothing to the analysis and could be an arbitrary expression.
Lastly, I note that all variables should be classified by unique identifiers even though they are not in the same scope - otherwise the interpreter performs a unionization of the ranges.

The code is based on the code given in the AP-course (specifically "week 3") - Thank you!
