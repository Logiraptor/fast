

type output =
    Group of output list
  | Item of string


type line = (int * string)


let rec lines_of_value (o : output) : line list =
    match o with
        Item s -> [(0, s)]
      | Group l ->
        let inc_indent (indent, s) = (indent + 1, s) in
        let sub_lines = List.map lines_of_value l in
        List.map inc_indent @@ List.flatten sub_lines

let string_of_line (indent, value) : string =
    String.make (indent * 4) ' ' ^ value

let value (o : output) : string =
    let lines = lines_of_value o in
    let strings = List.map string_of_line lines in
    String.concat "\n" strings

(*indent constructs a new string which is auto indented based on curly braces and semicolons.
  semicolons separate lines, curly braces denote structure.
*)
let string s =
    let level = ref 0 in
    let linestart = ref true in
    let outputBuffer = ref [] in
    let output s = outputBuffer := s :: !outputBuffer in
    let each c = 
        match c with
            '{' -> output (String.make !level '\t' ^ "{\n"); level := !level + 1; linestart := true
          | '}' -> level := !level - 1; output (String.make !level '\t' ^ "}\n")
          | ';' -> output (";"); linestart := true
          | '\t' -> ()
          | '\n' -> ()
          | c -> 
            if (!linestart) then
            (output ("\n" ^ (String.make !level '\t'));
            linestart := false;
            output (String.make 1 c))
            else output (String.make 1 c)
    in
    String.iter each s;
    String.concat "" (List.rev !outputBuffer)
