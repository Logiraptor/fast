(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Eof


let getError lexbuf =
    begin
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        (line, cnum, tok)
      end

let string_of_error (line, column, token) =
    Printf.sprintf "%d:%d %s" line column token

}

rule token = parse
    [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
    
    | '#' [^ '\n']*  { token lexbuf } (* skip comments *)
    
    | "val"          { VAL }
    
    | ['0'-'9']+ as lxm         { INT(int_of_string lxm) }
    | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '\'']* as lxm {ID lxm}
    
    
    | '='            { EQUALS }
    | ','            { COMMA }
    | "=>"           { ROCKET }
    | "=="           { EQUALSEQUALS }
    | "<"            { LESSTHAN }
    | ">"            { GREATERTHAN }
    | "<="           { LESSTHANEQUALS }
    | ">="           { GREATERTHANEQUALS }
    | '+'            { PLUS }
    | '-'            { MINUS }
    | '*'            { TIMES }
    | '/'            { DIV }
    | '('            { LPAREN }
    | ')'            { RPAREN }
    | eof            { EOF }

    | _ as c         { 
        let error = getError lexbuf in
        let errorString = string_of_error error in
        Printf.sprintf "%s unrecognized character %c" errorString c |> failwith
     }