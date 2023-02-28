open Code 
open OUnit2

let add_test name expected x y = name >:: fun _ ->
  assert_equal expected (add x y)

let add_tests = [add_test "" 5 3 2;
add_test "" 6 3 3]

  let tests =
    "test suite"
    >::: List.flatten
           [
             add_tests
           ]
  
  let _ = run_test_tt_main tests