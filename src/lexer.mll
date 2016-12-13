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

let next_line lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}

rule token = parse
    [' ' '\t']+     { token lexbuf }     (* skip blanks *)

    | ['\n' '\r']    { next_line lexbuf; token lexbuf }
    | '#' [^ '\n']*  { token lexbuf } (* skip comments *)
    
    | "val"          { VAL }
    | "if"           { IF }
    | "then"         { THEN }
    | "else"         { ELSE }
    
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