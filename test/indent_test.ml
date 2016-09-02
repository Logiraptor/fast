
open OUnit

let all = "Indent Tests" >::: [
    "noop case 2" >:: ( fun () -> 
        assert_equal ~printer:(fun x->x)
        "foo"
        (Indent.value (Indent.Item "foo"))
    );
    
    "single group" >:: (fun () -> 
        assert_equal ~printer:(fun x->x)
        "    a\n    b"
        (Indent.value (Indent.Group [
            Indent.Item "a";
            Indent.Item "b";
        ]))
    )
]

