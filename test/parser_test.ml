
open OUnit
open Ast
open Test_util

let all = "Parser" >:::
[
    "simple let" >:: ( fun () -> 
        parse "val a = 1" |>
        assert_equal_prog [("a", Int 1)]
    );
    "arithmetic" >:: ( fun () -> 
        parse "val x = 1 + 2" |>
        assert_equal_prog [("x", BinOp (Add, Int 1, Int 2))]
    );
    "identifier" >:: ( fun () -> 
        parse "val x = y2" |>
        assert_equal_prog [("x", ID "y2")]
    );
    "lambda" >:: ( fun () -> 
        parse "val id = x => x" |>
        assert_equal_prog [("id", Lambda ("x", ID "x"))]
    );
    "application" >:: ( fun () -> 
        parse "val main = run(arg)" |>
        assert_equal_prog [("main", Apply (ID "run", ID "arg"))]
    );
    "function definition" >:: ( fun () -> 
        parse "val fst(arg1, arg2) = arg1" |>
        assert_equal_prog [("fst", Lambda ("arg1", Lambda ("arg2", ID "arg1")))]
    );
    "precedence" >:: ( fun () -> 
        parse "val result = a + b * c" |>
        assert_equal_prog [("result", BinOp (Add, ID "a", BinOp (Mul, ID "b", ID "c")))]
    );
    "nested expressions" >:: ( fun () -> 
        parse "val result = (a + b) * (c * d(e) + f)" |>
        assert_equal_prog [("result", 
            BinOp (Mul, 
                BinOp (Add, ID "a", ID "b"), 
                BinOp (Add, 
                    BinOp (Mul, ID "c", Apply (ID "d", ID "e")), 
                    ID "f"
                ))
            )]
    );
    
    "full program" >:: ( fun () -> 
        parse "
# result just computes some stuff
val result = c * d(e)

# other adds 2 to whatever you give it
val other(arg) = arg + 2
" |>
        assert_equal_prog [
            ("result", BinOp (Mul, ID "c", Apply (ID "d", ID "e")));
            ("other", Lambda ("arg", BinOp (Add, ID "arg", Int 2)))
        ]
    )
]
