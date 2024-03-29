open Finance
open Account
open OUnit2
(**The following is our test plan for the Account module of the Finance library.
   The Account module is responsible for the creation and management of accounts
   in a banking system. The tests are developed using the OUnit testing
   framework, which is an automatic testing framework. The Account module will
   be also be tested manually, within the interface we created. The OUnit tests
   check for exceptions as well as ensure that account creation is executed
   properly. For any mutations to accounts, we needed to test manually through
   the interface, as OUnit runs tests concurrently, rather than sequentially. To
   verify mutable changes to accounts, we used query functions to confirm
   updates. We used a combination of black box testing and glass box testing.
   Black box testing was used to test the functions using their documentation
   specifications. We verified each function functioned as the specification
   ensured it would. Glass box testing was also used to test the functions of
   the module, as we  ensured that nearly all parts of the code paths within
   each function was explored. More specifically, we ensured that different
   branches, conditions, and loops within each function were tested. This
   allowed us to verify the correctness of the internal logic and handling of
   different scenarios within the module. By examining the code paths and
   executing tests that cover different branches, we can identify any potential
   bugs or unexpected behaviors in the implementation. This comprehensive
   testing approach provides a high level of confidence in the functionality and
   reliability of the Account module. Test cases are developed to verify that
   the functions of the module are producing expected outputs. The testing
   approach demonstrates the correctness of the system because the tests are
   designed to cover all possible inputs and edge cases. We tested the module
   with several different inputs. We also checked edge cases, such as when the
   account fields, such as balance and limit, are zero. Thus, we verified that
   it is producing the expected outputs. Furthermore, the testing approach
   ensures that the module is working as intended and is free from any bugs, as
   we also confirmed that functions raised exceptions when expected. This gives
   us confidence that the Account module will function as expected in the
   overall Finance library.  *)


let data_dir_prefix = "data" ^ Filename.dir_sep
let sample_yo = Yojson.Basic.from_file (data_dir_prefix ^ "Sample.json")
let sample = sample_yo |> from_json
let account1_id = create_account "Sam" "Savings" 5 6000 1000 500
let account2_id = create_account "James" "Checking" 0 200 25 150
let account3_id = create_account "Alex" "Credit" 10 20 5 10

(** [owner_test name expected acc] constructs an OUnit test named [name] that
    asserts the quality of [expected] with [owner acc]. *)
let owner_test (name : string) (expected : string) (acc : int) =
  name >:: fun _ -> assert_equal expected (Account.owner acc)

(** [acc_type_test name expected acc] constructs an OUnit test named [name] that
    asserts the quality of [expected] with [account_type acc]. *)
let acc_type_test (name : string) (expected : string) (acc : int) =
  name >:: fun _ -> assert_equal expected (Account.account_type acc)

(** [status_test name expected acc] constructs an OUnit test named [name] that
    asserts the quality of [expected] with [status acc]. *)
let status_test (name : string) (expected : string) (acc : int) =
  name >:: fun _ -> assert_equal expected (Account.status acc)

(** [balance_test name expected acc] constructs an OUnit test named [name] that
    asserts the quality of [expected] with [balance acc]. *)
let balance_test name expected acc =
  name >:: fun _ ->
  assert_equal expected (Account.balance acc) ~printer:string_of_int

(** [limit_test name expected acc] constructs an OUnit test named [name] that
    asserts the quality of [expected] with [limit acc]. *)
let limit_test name expected acc =
  name >:: fun _ ->
  assert_equal expected (Account.limit acc) ~printer:string_of_int

(** [maximum_test name expected acc] constructs an OUnit test named [name] that
    asserts the quality of [expected] with [maximum acc]. *)
let maximum_test name expected acc =
  name >:: fun _ ->
  assert_equal expected (Account.maximum acc) ~printer:string_of_int

(** [withdraw_raiseM name expected acc] constructs an OUnit test named [name]
    that asserts that [expected] is raised in [withdraw acc]. *)
let withdraw_raiseM name acc n =
  name >:: fun _ ->
  assert_raises (Account.MaximumExceeded n) (fun () -> Account.withdraw acc n)

(** [withdraw_raiseL name expected acc] constructs an OUnit test named [name]
    that asserts that [expected] is raised in [withdraw acc]. *)
let withdraw_raiseL name acc n =
  name >:: fun _ ->
  assert_raises (Account.LimitExceeded n) (fun () -> Account.withdraw acc n)

(** [withdraw_raiseINA name expected acc] constructs an OUnit test named [name]
    that asserts that [expected] is raised in [withdraw acc]. *)
let withdraw_raiseINA name acc n =
  name >:: fun _ ->
  assert_raises Account.InactiveAccount (fun () -> Account.withdraw acc n)

(** [withdraw_raiseI name expected acc] constructs an OUnit test named [name]
    that asserts that [expected] is raised in [withdraw acc]. *)
let withdraw_raiseI name acc n =
  name >:: fun _ ->
  assert_raises Account.InsufficientFunds (fun () -> Account.withdraw acc n)

(** [transfer_raiseM name expected acc] constructs an OUnit test named [name]
    that asserts that [expected] is raised in [transfer acc]. *)
let transfer_raiseM name acc1 acc2 n =
  name >:: fun _ ->
  assert_raises (Account.MaximumExceeded n) (fun () ->
      Account.transfer acc1 acc2 n)

(** [transfer_raiseL name expected acc] constructs an OUnit test named [name]
    that asserts that [expected] is raised in [transfer acc]. *)
let transfer_raiseL name acc1 acc2 n =
  name >:: fun _ ->
  assert_raises (Account.LimitExceeded n) (fun () ->
      Account.transfer acc1 acc2 n)

(** [transfer_raiseI name expected acc] constructs an OUnit test named [name]
    that asserts that [expected] is raised in [transfer acc]. *)
let transfer_raiseI name acc1 acc2 n =
  name >:: fun _ ->
  assert_raises Account.InsufficientFunds (fun () ->
      Account.transfer acc1 acc2 n)

(** [latest_transaction_test name expected acc] constructs an OUnit test named
    [name] that asserts the quality of [expected] with [latest_transaction acc]. *)
let latest_transaction_test name expected acc =
  name >:: fun _ -> assert_equal expected (Account.latest_transaction acc)

(** [all_transactions_test name expected acc] constructs an OUnit test named
    [name] that asserts the quality of [expected] with [all_transactions acc]. *)
let all_transactions_test name expected acc =
  name >:: fun _ -> assert_equal expected (Account.all_transactions acc)

(** [stocks_value_test name expected acc] constructs an OUnit test named [name]
    that asserts the quality of [expected] with [stocks_value acc]. *)
let stocks_value_test name expected acc =
  name >:: fun _ ->
  assert_equal expected (Account.stocks_value acc) ~printer:string_of_int

(** [transactions_value_test name expected acc] constructs an OUnit test named
    [name] that asserts the quality of [expected] with [transactions_value acc]. *)
let transactions_value_test name expected acc =
  name >:: fun _ ->
  assert_equal expected (Account.transactions_value acc) ~printer:string_of_int

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
    latest_transaction_test "latest_transaction sample"
      "Transaction Type: Deposit, Amount: 70" sample;
    latest_transaction_test "latest_transaction account1" "No transactions made"
      account1_id;
    latest_transaction_test "latest_transaction account2" "No transactions made"
      account2_id;
    latest_transaction_test "latest_transaction account3" "No transactions made"
      account3_id;
    all_transactions_test "all_transactions sample"
      [
        "Transaction Type: Deposit, Amount: 70";
        "Transaction Type: Withdrawal, Amount: -50";
      ]
      sample;
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
    stocks_value_test "stocks_value sample" 13100 sample;
    stocks_value_test "stocks_value account1" 0 account1_id;
    stocks_value_test "stocks_value account2" 0 account2_id;
    stocks_value_test "stocks_value account3" 0 account3_id;
    transactions_value_test "transactions_value sample" 20 sample;
    transactions_value_test "transactions_value account1" 0 account1_id;
    transactions_value_test "transactions_value account2" 0 account2_id;
    transactions_value_test "transactions_value account3" 0 account3_id;
  ]

(** [projected_balance_test name expected acc] constructs an OUnit test named
    [name] that asserts the quality of [expected] with
    [yearly_projected_balance acc]. *)
let projected_balance_test (name : string) (expected : int) (acc : int) =
  name >:: fun _ -> assert_equal expected (Account.yearly_projected_balance acc)

(** [approved_mortgage_test name expected i prop_id mortgage_value] constructs
    an OUnit test named [name] that asserts the quality of [expected] with
    [approved_mortgage i prop_id mortgage_value]. *)
let approved_mortgage_test (name : string) (expected : string) (i : int)
    (prop_id : int) (mortgage_value : int) =
  name >:: fun _ ->
  assert_equal expected (Account.approved_mortgage i prop_id mortgage_value)

(** [get_mortgage_test name expected i prop_id mortgage_value] constructs an
    OUnit test named [name] that asserts the quality of [expected] with
    [get_mortgage i prop_id mortgage_value]. *)
let get_mortgage_test (name : string) (expected : string) (i : int)
    (prop_id : int) (mortgage_value : int) =
  name >:: fun _ ->
  assert_equal expected (Account.get_mortgage i prop_id mortgage_value)

(** [identify_property_helper_test name expected properties prop_id] constructs
    an OUnit test named [name] that asserts the quality of [expected] with
    [identify_property_helper properties prop_id]. *)
let identify_property_helper_test (name : string) (expected : property)
    (properties : property list) (prop_id : int) =
  name >:: fun _ ->
  assert_equal expected (Account.identify_property_helper properties prop_id)

(** [identify_property_helper_failure_tests name expected properties prop_id]
    constructs an OUnit test named [name] that asserts the quality of [expected]
    with [identify_property_helper properties prop_id]. *)
let identify_property_helper_failure_test (name : string) (expected : exn)
    (properties : property list) (prop_id : int) =
  name >:: fun _ ->
  assert_raises expected (fun () ->
      Account.identify_property_helper properties prop_id)

(** [identify_property_failure_test name expected acc] constructs an OUnit test
    named [name] that asserts the quality of [expected] with
    [identify_property i prop_id]. *)
let identify_property_failure_test (name : string) (expected : exn) (i : int)
    (prop_id : int) =
  name >:: fun _ ->
  assert_raises expected (fun () -> Account.identify_property i prop_id)

(** [remove_property_helper_test properties prop_id acc] constructs an OUnit
    test named [name] that asserts the quality of [expected] with
    [remove_property_helper properties prop_id acc]. *)
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
