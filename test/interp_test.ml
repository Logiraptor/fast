
open OUnit
open Test_util
open Llvm_executionengine


let load src =
    parse src |> Alpha.convert


let cmp_values a b =
    GenericValue.as_int a == GenericValue.as_int b

let assert_equal_value a b =
    assert_equal ~cmp:cmp_values a b


let i x = GenericValue.of_int Codegen.int_type x


let make_test name (source, expected) =
    (name ^ " " ^ source) >:: ( fun () ->
        let prog = load source in
        let value = Codegen.interp prog in
        assert_equal_value expected value
    )


let test_group name sub_tests =
    List.map (make_test name) sub_tests

let all = "Interp" >:::
[
    "basic interp" >:: ( fun () ->
        let prog = load "val main(x) = 0" in
        let value = Codegen.interp prog in
        assert_equal_value (i 0) value
    )] @
    test_group "arithmetic" [
        ("val main(x) = 0", i 0);
        ("val main(x) = 1", i 1);
        ("val main(x) = 100", i 100); 
        ("val main(x) = 1 + 2", i 3); 
        ("val main(x) = 1 + 2 * 6", i 13); 
        ("val main(x) = (1 + 2 * (6 / 2)) - 3", i 4); 
    ] @
    test_group "lambdas" [
        ("val id = x => x
          val main(x) = id(1)", i 1);
        ("val add(a) = a + 1
          val main(x) = add(5)", i 6);
        (*("val sum(a, b) = a + b
          val apply(f, x) = f(x)
          val main(x) = apply(sum(50), 25)", i 75)*)
    ] @
    test_group "boolean operators" [
        ("val main(x) = 1 == 1", i 1);
        ("val main(x) = 1 == 2", i 0);
        ("val main(x) = 1 < 0", i 0);
        ("val main(x) = 1 < 1", i 0);
        ("val main(x) = 1 < 2", i 1);
        ("val main(x) = 1 > 2", i 0);
        ("val main(x) = 1 > 1", i 0);
        ("val main(x) = 1 > 0", i 1);
        ("val main(x) = 1 >= 2", i 0);
        ("val main(x) = 1 >= 1", i 1);
        ("val main(x) = 1 >= 0", i 1);
        ("val main(x) = 1 > 0", i 1);
        ("val main(x) = 1 <= 2", i 1);
        ("val main(x) = 1 <= 1", i 1);
        ("val main(x) = 1 <= 0", i 0);
    ] @
    test_group "conditional logic" [
        ("val main(x) = if 1 == 1 then 0 else 1", i 0);
        ("val main(x) = if 1 == 2 then 0 else 1", i 1);
    ] @
    test_group "recursive function" [
           ("val rec(x) = if x == 0 then 2 else rec(x-1)
             val main(x) = rec(1)", i 2);
           ("val rec(x) = if x == 0 then 2 else rec(x-1)
             val main(x) = rec(2)", i 2);
    ]


