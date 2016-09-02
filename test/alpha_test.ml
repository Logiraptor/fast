open OUnit
open Ast
open Test_util

let assert_equal_convert a b =
    let parseA = parse a in
    let parseB = parse b in
    Alpha.convert parseA |>
    assert_equal_prog parseB

let all = "Alpha Conversion" >:::
[
    "noop" >:: ( fun () ->
        let noop_prog = "
val a = 1
val b = a
val c(d) = b(a)
val e = f => f
val g = h => g
        " in 
        assert_equal_convert noop_prog noop_prog
    );
    "lambda" >:: ( fun () ->
        assert_equal_convert
        "val x = x => x"
        "val x = x' => x'"
    );
    "call" >:: ( fun () ->
        assert_equal_convert
        "val bar = foo => bar => foo(bar)"
        "val bar = foo => bar' => foo(bar')"
    );

    "redefine" >:: ( fun () ->
        assert_raises (Alpha.RedefinedID "a")
            (fun () -> Alpha.convert (parse 
        "val a = 1
         val a = 2"))
    );
    "undefined id" >:: ( fun () -> 
        assert_raises (Alpha.UndefinedID "foo") 
            (fun () -> Alpha.convert (parse "val x = foo"))
    )
]
