open OUnit


let _ = 
    begin
        run_test_tt Parser_test.parser_tests
    end