open OUnit


let _ = 
    run_test_tt_main Parser_test.all;
    run_test_tt_main Alpha_test.all;
    run_test_tt_main Interp_test.all;
    run_test_tt_main Indent_test.all
