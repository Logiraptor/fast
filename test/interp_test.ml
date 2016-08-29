
open OUnit
open Test_util


let load src =
    parse src |> Alpha.convert


let assert_equal_value a b =
    assert_equal ~printer:Interp.string_of_value a b


let all = "Interp" >:::
[
    "basic interp" >:: ( fun () ->
        let prog = load "val main(x) = 0" in
        let value = Interp.interp prog in
        assert_equal_value (Interp.Int 0) value
    );
    "arithmetic" >:: ( fun () ->
        List.iter ( fun (src, value) -> 
            let prog = load src in
            let actual = Interp.interp prog in
            assert_equal_value value actual
        ) [
           ("val main(x) = 0", Interp.Int 0);
           ("val main(x) = 1", Interp.Int 1);
           ("val main(x) = 100", Interp.Int 100); 
           ("val main(x) = 1 + 2", Interp.Int 3); 
           ("val main(x) = 1 + 2 * 6", Interp.Int 13); 
           ("val main(x) = (1 + 2 * (6 / 2)) - 3", Interp.Int 4); 
        ]
    );
    "lambdas" >:: ( fun () ->
        List.iter ( fun (src, value) -> 
            let prog = load src in
            let actual = Interp.interp prog in
            assert_equal_value value actual
        ) [
           ("val id = x => x
             val main(x) = id(1)", Interp.Int 1);
           ("val add(a) = a + 1
             val main(x) = add(5)", Interp.Int 6);
           ("val sum(a, b) = a + b
             val apply(f, x) = f(x)
             val main(x) = apply(sum(50), 25)", Interp.Int 75)
        ]
    );
    "boolean operators" >:: ( fun () ->
        List.iter ( fun (src, value) -> 
            let prog = load src in
            let actual = Interp.interp prog in
            assert_equal_value value actual
        ) [
           ("val main(x) = 1 == 1", Interp.True);
           ("val main(x) = 1 == 2", Interp.False);
           ("val main(x) = 1 < 0", Interp.False);
           ("val main(x) = 1 < 1", Interp.False);
           ("val main(x) = 1 < 2", Interp.True);
           ("val main(x) = 1 > 2", Interp.False);
           ("val main(x) = 1 > 1", Interp.False);
           ("val main(x) = 1 > 0", Interp.True);
           ("val main(x) = 1 >= 2", Interp.False);
           ("val main(x) = 1 >= 1", Interp.True);
           ("val main(x) = 1 >= 0", Interp.True);
           ("val main(x) = 1 > 0", Interp.True);
           ("val main(x) = 1 <= 2", Interp.True);
           ("val main(x) = 1 <= 1", Interp.True);
           ("val main(x) = 1 <= 0", Interp.False);
        ]
    );
]

