open Finance
open Account
open OUnit2

let data_dir_prefix = "data" ^ Filename.dir_sep
let sample_yo = Yojson.Basic.from_file (data_dir_prefix ^ "Sample.json")
let sample = sample_yo |> from_json
let account1_id = create_account "Sam" "Savings" 5 6000 1000 500
let account2_id = create_account "James" "Checking" 0 200 0 150
let account3_id = create_account "Alex" "Credit" 10 0 5 0

let owner_test (name : string) (expected : string) (acc : int) =
  name >:: fun _ -> assert_equal expected (Account.owner acc)

let acc_type_test (name : string) (expected : string) (acc : int) =
  name >:: fun _ -> assert_equal expected (Account.account_type acc)

let status_test (name : string) (expected : string) (acc : int) =
  name >:: fun _ -> assert_equal expected (Account.status acc)

let balance_test name expected acc =
  name >:: fun _ -> assert_equal expected (Account.balance acc)

let limit_test name expected acc =
  name >:: fun _ -> assert_equal expected (Account.limit acc)

let maximum_test name expected acc =
  name >:: fun _ -> assert_equal expected (Account.maximum acc)


let withdraw_test name expected acc n =
  name >:: fun _ -> assert_equal expected (Account.withdraw acc n)

let withdraw_raise name expected acc n =
  name >:: fun _ -> assert_raises expected (fun () -> Account.withdraw acc n)

let deposit_test name expected acc n =
  name >:: fun _ -> assert_equal expected (Account.deposit acc n)

let activate_test name expected acc n =
  name >:: fun _ -> assert_equal expected (Account.activate acc)

let deactivate_test name expected acc n =
  name >:: fun _ -> assert_equal expected (Account.deactivate acc)

let transfer_test name expected acc1 acc2 n =
  name >:: fun _ -> assert_equal expected (Account.transfer acc1 acc2 n)

(*withdraw,deposit,activate,deactivate,transfer*)

let owner_tests =
  [
    owner_test "sample owner" "Juice Washington" sample;
    owner_test "account1 owner" "Sam" account1_id;
    owner_test "account2 owner" "James" account2_id;
    owner_test "account3 owner" "Alex" account3_id;
    acc_type_test "sample account type" "Checking" sample;
    acc_type_test "account1 account type" "Savings" account1_id;
    acc_type_test "account2 account type" "Checking" account2_id;
    acc_type_test "account3 account type" "Credit" account3_id;
    status_test "sample status" "Active" sample;
    status_test "account1 status" "Active" account1_id;
    status_test "account2 status" "Active" account2_id;
    status_test "account3 status" "Active" account3_id;
    balance_test "sample balance" 100 sample;
    balance_test "account1 balance" 6000 account1_id;
    balance_test "account2 balance" 200 account2_id;
    balance_test "account3 balance" 0 account3_id;
    limit_test "sample limit" 10 sample;
    limit_test "account1 limit" 1000 account1_id;
    limit_test "account2 limit" 0 account2_id;
    limit_test "account3 limit" 5 account3_id;
    maximum_test "sample maximum" 250 sample;
    maximum_test "account1 maximum" 500 account1_id;
    maximum_test "account2 maximum" 150 account2_id;
    maximum_test "account3 maximum" 0 account3_id;
    withdraw_test "sample withdraw" () sample 10;
    (*withdraw_raise "MaximumExceeded" (MaximumExceeded 11) sample 11;*)
  ]

let accounta_id = create_account "Sam" "Savings" 500 600000 1000 500

(* owner acc_type interest balance limit maximum *)
let accountb_id = create_account "James" "Checking" 0 20000 0 150
let accountc_id = create_account "Alex" "Credit" 1000 0 5 0

let projected_balance_test (name : string) (expected : int) (acc : int) =
  name >:: fun _ -> assert_equal expected (Account.yearly_projected_balance acc)

let property_tests =
  [
    projected_balance_test "balance on Sam" 630000 accounta_id;
    projected_balance_test "balance on James" 20000 accountb_id;
    projected_balance_test "balance on James" 0 accountc_id;
  ]

let tests = "test suite" >::: List.flatten [ owner_tests; property_tests ]
let _ = run_test_tt_main tests
