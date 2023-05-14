open Finance
open Account
open OUnit2

let data_dir_prefix = "data" ^ Filename.dir_sep
let sample_yo = Yojson.Basic.from_file (data_dir_prefix ^ "Sample.json")
let sample = sample_yo |> from_json
let account1_id = create_account "Sam" "Savings" 5 6000 1000 500
let account2_id = create_account "James" "Checking" 0 200 25 150
let account3_id = create_account "Alex" "Credit" 10 20 5 10

let owner_test (name : string) (expected : string) (acc : int) =
  name >:: fun _ -> assert_equal expected (Account.owner acc)

let acc_type_test (name : string) (expected : string) (acc : int) =
  name >:: fun _ -> assert_equal expected (Account.account_type acc)

let status_test (name : string) (expected : string) (acc : int) =
  name >:: fun _ -> assert_equal expected (Account.status acc)

let balance_test name expected acc =
  name >:: fun _ ->
  assert_equal expected (Account.balance acc) ~printer:string_of_int

let limit_test name expected acc =
  name >:: fun _ ->
  assert_equal expected (Account.limit acc) ~printer:string_of_int

let maximum_test name expected acc =
  name >:: fun _ ->
  assert_equal expected (Account.maximum acc) ~printer:string_of_int

let withdraw_raiseM name acc n =
  name >:: fun _ ->
  assert_raises (Account.MaximumExceeded n) (fun () -> Account.withdraw acc n)

let withdraw_raiseL name acc n =
  name >:: fun _ ->
  assert_raises (Account.LimitExceeded n) (fun () -> Account.withdraw acc n)

let withdraw_raiseINA name acc n =
  name >:: fun _ ->
  assert_raises Account.InactiveAccount (fun () -> Account.withdraw acc n)

let withdraw_raiseI name acc n =
  name >:: fun _ ->
  assert_raises Account.InsufficientFunds (fun () -> Account.withdraw acc n)

let transfer_raiseM name acc1 acc2 n =
  name >:: fun _ ->
  assert_raises (Account.MaximumExceeded n) (fun () ->
      Account.transfer acc1 acc2 n)

let transfer_raiseL name acc1 acc2 n =
  name >:: fun _ ->
  assert_raises (Account.LimitExceeded n) (fun () ->
      Account.transfer acc1 acc2 n)

let transfer_raiseI name acc1 acc2 n =
  name >:: fun _ ->
  assert_raises Account.InsufficientFunds (fun () ->
      Account.transfer acc1 acc2 n)

let latest_transaction_test name expected acc =
  name >:: fun _ -> assert_equal expected (Account.latest_transaction acc)

let all_transactions_test name expected acc =
  name >:: fun _ -> assert_equal expected (Account.all_transactions acc)

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
    balance_test "account3 balance" 20 account3_id;
    limit_test "sample limit" 10 sample;
    limit_test "account1 limit" 1000 account1_id;
    limit_test "account2 limit" 25 account2_id;
    limit_test "account3 limit" 5 account3_id;
    maximum_test "sample maximum" 250 sample;
    maximum_test "account1 maximum" 500 account1_id;
    maximum_test "account2 maximum" 150 account2_id;
    maximum_test "account3 maximum" 10 account3_id;
    withdraw_raiseM "Max Exceed account1" account1_id 501;
    withdraw_raiseM "Max Exceed account2" account2_id 151;
    withdraw_raiseM "Max Exceed account3" account3_id 11;
    withdraw_raiseI "Insufficient Funds sample" sample 127;
    withdraw_raiseI "Insufficient Funds account1" account1_id 7000;
    withdraw_raiseI "Insufficient Funds account2" account2_id 567;
    withdraw_raiseI "Insufficient Funds account3" account3_id 21;
    latest_transaction_test "latest_transaction sample" "No transactions made"
      sample;
    latest_transaction_test "latest_transaction account1" "No transactions made"
      account1_id;
    latest_transaction_test "latest_transaction account2" "No transactions made"
      account2_id;
    latest_transaction_test "latest_transaction account3" "No transactions made"
      account3_id;
    all_transactions_test "all_transactions sample" [] sample;
    all_transactions_test "all_transactions account1" [] account1_id;
    all_transactions_test "all_transactions account2" [] account2_id;
    all_transactions_test "all_transactions account3" [] account3_id;
    transfer_raiseM "transfer maximum exceeded account1 account2" account1_id
      account2_id 501;
    transfer_raiseM "transfer maximum exceeded account2 account3" account2_id
      account3_id 151;
    transfer_raiseM "transfer maximum exceeded account3 account2" account3_id
      account2_id 11;
    transfer_raiseL "transfer limit exception sample account1" sample
      account1_id 91;
    transfer_raiseI "transfer insufficient funds sample account2" sample
      account2_id 127;
    transfer_raiseI "transfer insufficient funds account1 account2" account1_id
      account2_id 7000;
    transfer_raiseI "transfer insufficient funds account2 account3" account2_id
      account3_id 567;
    transfer_raiseI "transfer insufficient funds account3 account2" account3_id
      account2_id 21;
  ]

let accounta_id = create_account "Sam" "Savings" 500 600000 1000 500

(* owner acc_type interest balance limit maximum *)

let projected_balance_test (name : string) (expected : int) (acc : int) =
  name >:: fun _ -> assert_equal expected (Account.yearly_projected_balance acc)

(* i prop_id mortgage_value*)
let approved_mortgage_test (name : string) (expected : string) (i : int)
    (prop_id : int) (mortgage_value : int) =
  name >:: fun _ ->
  assert_equal expected (Account.approved_mortgage i prop_id mortgage_value)

let get_mortgage_test (name : string) (expected : string) (i : int)
    (prop_id : int) (mortgage_value : int) =
  name >:: fun _ ->
  assert_equal expected (Account.get_mortgage i prop_id mortgage_value)

let identify_property_helper_test (name : string) (expected : property)
    (properties : property list) (prop_id : int) =
  name >:: fun _ ->
  assert_equal expected (Account.identify_property_helper properties prop_id)

let identify_property_helper_failure_test (name : string) (expected : exn)
    (properties : property list) (prop_id : int) =
  name >:: fun _ ->
  assert_raises expected (fun () ->
      Account.identify_property_helper properties prop_id)

let identify_property_failure_test (name : string) (expected : exn) (i : int)
    (prop_id : int) =
  name >:: fun _ ->
  assert_raises expected (fun () -> Account.identify_property i prop_id)

let remove_property_helper_test (name : string) (expected : property list)
    (properties : property list) (prop_id : int) (acc : property list) =
  name >:: fun _ ->
  assert_equal expected (Account.remove_property_helper properties prop_id acc)

let property_tests =
  [
    projected_balance_test "$6000 with 5% interest" 630000
      (create_account "Sam" "Savings" 500 600000 1000 500);
    projected_balance_test "$200 with 0% interest" 20000
      (create_account "James" "Checking" 0 20000 0 150);
    projected_balance_test "$0 with 10% interest" 0
      (create_account "Alex" "Credit" 1000 0 0 0);
    approved_mortgage_test "Approve a mortgage #1" "Approved Mortgage"
      (create_account "Sam" "Savings" 500 10 0 500)
      1 10;
    approved_mortgage_test "Approve a mortgage #2" "Approved Mortgage"
      (create_account "Sam" "Savings" 500 1000000 0 500)
      1 1000;
    approved_mortgage_test "Approve a mortgage #3" "Approved Mortgage"
      (create_account "Sam" "Savings" 500 1000000 0 1000000)
      1 1000000;
    get_mortgage_test "Approve a mortgage (get) #1" "Approved Mortgage"
      (create_account "Sam" "Savings" 500 10 0 500)
      1 10;
    get_mortgage_test "Approve a mortgage (get) #2" "Approved Mortgage"
      (create_account "Sam" "Savings" 500 1000000 0 500)
      1 1000;
    get_mortgage_test "Approve a mortgage (get) #3" "Approved Mortgage"
      (create_account "Sam" "Savings" 500 1000000 0 5000000)
      1 4999999;
    get_mortgage_test "Fail a mortgage (get) #1"
      "Not Enough Funds for Said Property"
      (create_account "Sam" "Savings" 500 10 0 500)
      1 1000;
    get_mortgage_test "Fail a mortgage (get) #2"
      "Not Enough Funds for Said Property"
      (create_account "Sam" "Savings" 500 0 0 500)
      1 10000000;
    get_mortgage_test "Fail a mortgage (get) #3"
      "Not Enough Funds for Said Property"
      (create_account "Sam" "Savings" 500 1000000 0 250000)
      1 5000000;
    identify_property_helper_test "Identifying the first property"
      {
        id = 1;
        remaining_mortgage = 0;
        mortgage_monthly_cost = 0;
        current_rental_income = 0;
        hoa_upkeep_and_other_expenses = 0;
      }
      [
        {
          id = 1;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 0;
          hoa_upkeep_and_other_expenses = 0;
        };
      ]
      1;
    identify_property_helper_test "Identifying the second property"
      {
        id = 2;
        remaining_mortgage = 0;
        mortgage_monthly_cost = 0;
        current_rental_income = 500;
        hoa_upkeep_and_other_expenses = 0;
      }
      [
        {
          id = 1;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 0;
          hoa_upkeep_and_other_expenses = 0;
        };
        {
          id = 2;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 500;
          hoa_upkeep_and_other_expenses = 0;
        };
      ]
      2;
    identify_property_helper_test
      "Identifying a property in the middle of the list"
      {
        id = 2;
        remaining_mortgage = 0;
        mortgage_monthly_cost = 0;
        current_rental_income = 500;
        hoa_upkeep_and_other_expenses = 0;
      }
      [
        {
          id = 1;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 0;
          hoa_upkeep_and_other_expenses = 0;
        };
        {
          id = 2;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 500;
          hoa_upkeep_and_other_expenses = 0;
        };
        {
          id = 3;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 500;
          hoa_upkeep_and_other_expenses = 0;
        };
        {
          id = 4;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 500;
          hoa_upkeep_and_other_expenses = 0;
        };
      ]
      2;
    identify_property_helper_failure_test
      "Identifying a property in the middle of the list"
      (Failure "Property does not exist")
      [
        {
          id = 1;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 0;
          hoa_upkeep_and_other_expenses = 0;
        };
        {
          id = 2;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 500;
          hoa_upkeep_and_other_expenses = 0;
        };
        {
          id = 3;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 500;
          hoa_upkeep_and_other_expenses = 0;
        };
        {
          id = 4;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 500;
          hoa_upkeep_and_other_expenses = 0;
        };
      ]
      5;
    identify_property_failure_test "No home here"
      (Failure "Property does not exist")
      (create_account "Alex" "Credit" 1000 0 0 0)
      1;
    remove_property_helper_test "remove first property in a list" []
      [
        {
          id = 1;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 0;
          hoa_upkeep_and_other_expenses = 0;
        };
      ]
      1 [];
    remove_property_helper_test "remove the middle property in a list"
      [
        {
          id = 1;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 0;
          hoa_upkeep_and_other_expenses = 0;
        };
        {
          id = 3;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 0;
          hoa_upkeep_and_other_expenses = 0;
        };
      ]
      [
        {
          id = 1;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 0;
          hoa_upkeep_and_other_expenses = 0;
        };
        {
          id = 2;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 0;
          hoa_upkeep_and_other_expenses = 0;
        };
        {
          id = 3;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 0;
          hoa_upkeep_and_other_expenses = 0;
        };
      ]
      2 [];
    remove_property_helper_test "remove none of the properties in a list"
      [
        {
          id = 1;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 0;
          hoa_upkeep_and_other_expenses = 0;
        };
        {
          id = 2;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 0;
          hoa_upkeep_and_other_expenses = 0;
        };
        {
          id = 3;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 0;
          hoa_upkeep_and_other_expenses = 0;
        };
      ]
      [
        {
          id = 1;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 0;
          hoa_upkeep_and_other_expenses = 0;
        };
        {
          id = 2;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 0;
          hoa_upkeep_and_other_expenses = 0;
        };
        {
          id = 3;
          remaining_mortgage = 0;
          mortgage_monthly_cost = 0;
          current_rental_income = 0;
          hoa_upkeep_and_other_expenses = 0;
        };
      ]
      4 [];
  ]

let tests = "test suite" >::: List.flatten [ owner_tests; property_tests ]
let _ = run_test_tt_main tests
