open Finance
open Account
open OUnit2

let data_dir_prefix = "data" ^ Filename.dir_sep
let sample_yo = Yojson.Basic.from_file (data_dir_prefix ^ "Sample.json")
let sample = sample_yo |> from_json

let owner_test name expected acc =
  name >:: fun _ -> assert_equal expected (Account.owner sample)

let owner_tests =
  [
    owner_test "sample owner" "Johnny" sample;
    owner_test "sample owner" "Johnny" sample;
  ]

let tests = "test suite" >::: List.flatten [ owner_tests ]
let _ = run_test_tt_main tests
