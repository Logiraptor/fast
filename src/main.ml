
let _ =
    let fileName = Array.get Sys.argv 1 in
    let inputFile = open_in fileName in
    let lexbuf = Lexing.from_channel inputFile in
    try
        begin
            let result = Parser.main Lexer.token lexbuf in
            let aconvertResult = Alpha.convert result in
            let output = Codegen.interp aconvertResult in
            close_in inputFile;
            exit output
        end
    with
    | Parser.Error ->
      begin
        let error = Lexer.getError lexbuf in
        close_in_noerr inputFile;
        Lexer.string_of_error error |> failwith
      end
    | e ->
        close_in_noerr inputFile;
        match e with
        | Interp.UndefinedID (pos, id) -> Printf.printf "line %d: undefined id (%s)\n" pos.pos_lnum id; ()
        | _ -> raise e
