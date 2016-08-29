
open OUnit
open Dict

type test_dict = (int, int) dict

let print_int_option o =
    match o with
        None -> "none"
      | Some x -> Printf.sprintf "some %d" x

let all = "Dict" >:::
[
    "empty lookup" >:: ( fun () ->
        assert_equal ~printer:print_int_option None (lookup Empty 1)
    );
    "non-empty lookup" >:: ( fun () ->
        let (e : test_dict) = append Empty 1 2  in
        assert_equal ~printer:print_int_option (Some 2) (lookup e 1)
    );
    "dict dump" >:: ( fun () ->
        let (e : test_dict) = append (append Empty 1 2) 2 3  in
        assert_equal ~printer:(fun x -> x) ("{2 -> 3; 1 -> 2; }") 
            (dump e string_of_int string_of_int)
    )
]
