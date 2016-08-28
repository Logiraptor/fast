
open OUnit

let parse s =
    let lexbuf = Lexing.from_string s in
    Parser.main Lexer.token lexbuf


let parser_tests = "Parser" >:::
[
    "simple let" >:: ( fun () -> 
        let result = parse "val a = 1" in
        assert_equal [("a", Ast.Int 1)] result
    )
]



