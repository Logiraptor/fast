
let _ =
    let inputFile = open_in "example.fb" in
    let lexbuf = Lexing.from_channel inputFile in
    try
        begin
            let result = Parser.main Lexer.token lexbuf in
            let aconvertResult = Alpha.convert result in
            let output = Ast.dump_program aconvertResult in
            print_string output; print_newline();
            close_in inputFile
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
        raise e
