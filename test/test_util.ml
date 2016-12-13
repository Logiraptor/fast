open OUnit

let parse s =
    let
        lexbuf = Lexing.from_string s
    in
        Parser.main Lexer.token lexbuf

let rec strip_pos e =
    match e with
      Ast.Pos (e, _) -> strip_pos e
    | Ast.BinOp (op, lhs, rhs) -> Ast.BinOp (op, strip_pos lhs, strip_pos rhs)
    | Ast.UnOp (op, inner) -> Ast.UnOp (op, strip_pos inner) 
    | Ast.Lambda (arg, body) -> Ast.Lambda (arg, strip_pos body) 
    | Ast.Apply (func, arg) -> Ast.Apply (strip_pos func, strip_pos arg)
    | Ast.If (cond, conseq, alt) -> Ast.If (strip_pos cond, strip_pos conseq, strip_pos alt)
    | _ -> e

let strip_pos_prog prog =
    let f (name, e) = (name, strip_pos e) in
    List.map f prog

let assert_equal_prog a b =
    assert_equal ~printer:Ast.dump_program (strip_pos_prog a) (strip_pos_prog b)
