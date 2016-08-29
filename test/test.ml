open OUnit


let _ = 
    run_test_tt_main Parser_test.all;
    run_test_tt_main Alpha_test.all;
    run_test_tt_main Dict_test.all
