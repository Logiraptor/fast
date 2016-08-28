open OUnit


let _ = 
    begin
        run_test_tt_main Parser_test.all;
        run_test_tt_main Alpha_test.all
    end
