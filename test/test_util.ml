open OUnit

let parse s =
    let
        lexbuf = Lexing.from_string s
    in
        Parser.main Lexer.token lexbuf


let assert_equal_prog a b =
    assert_equal ~printer:Ast.dump_program a b
