(* Utilize Lwt (open) *)
open Yojson.Basic.Util
open Yojson.Basic
open Lwt

let direc_file_prefix = "data" ^ Filename.dir_sep
let file_name_ref : string ref = ref ""

let help_str =
  "\"get balance\" \n\
   \"get owner\" \n\
   \"get account type\" \n\
   \"get interest rate\" \n\
   \"get account status\" \n\
   \"get limit\" \"get max\" \n\
   \"withdraw\" \n\
   \"deposit\" \n\
   \"get stocks value\" \n\
   \"start transfer\" \n\
   \"get projected balance\" \n\
   \"mortgage check\" \n\
   \"get properties\" \n\
   \"remove properties\" \n\
   \"get last transaction\" \n\
   \"get transactions\" \n\
   \"set property rent\" \n\
   \"set property expenses\" \n\
   \"quit\" \n\
   \"return\""

let rec wait_fun start_time seconds =
  let current_time = Unix.gettimeofday () in
  let elapsed_time = current_time -. start_time in
  if elapsed_time >= seconds then () else wait_fun start_time seconds

let wait seconds =
  print_endline "";
  let start_time = Unix.gettimeofday () in
  wait_fun start_time seconds

let wait_alt seconds =
  ANSITerminal.print_string [ ANSITerminal.green ] "";
  let start_time = Unix.gettimeofday () in
  wait_fun start_time seconds

let quit () =
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.red ] "Quitting";
  wait_alt 0.2;
  ANSITerminal.print_string [ ANSITerminal.red ] ".";
  wait_alt 0.2;
  ANSITerminal.print_string [ ANSITerminal.red ] ".";
  wait_alt 0.2;
  ANSITerminal.print_string [ ANSITerminal.red ] ".";
  wait_alt 0.2;
  ANSITerminal.print_string [ ANSITerminal.red ] ".";
  print_endline "";
  exit 0

let write_to_newfile acc_id =
  print_endline "Please enter a name for the file: ";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "quit" -> quit ()
  | file_name ->
      wait_alt 0.5;
      let json = Finance.JsonAccount.acc_to_json acc_id in
      let out_channel = open_out (direc_file_prefix ^ file_name ^ ".json") in
      to_channel out_channel json;
      close_out out_channel;
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.green ]
        ("Saved file as " ^ file_name ^ ".json")

let write_to_file acc_id =
  wait_alt 0.5;
  let json = Finance.JsonAccount.acc_to_json acc_id in
  to_file !file_name_ref json;
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.green ] "Saved file"

let rec save_to_json account =
  print_endline "";
  print_endline "Would you like to save to a new file? (y/n)";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "y" ->
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.green ] "Saving......";
      print_endline "";
      print_endline "";
      write_to_newfile account
  | "n" ->
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.green ] "Saving......";
      print_endline "";
      print_endline "";
      write_to_file account
  | _ ->
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.red ]
        "⛔ Please enter a correct command ⛔";
      print_endline "";
      print_endline "";
      save_to_json account

let rec save account =
  print_endline "";
  print_endline "Would you like to save your changes? (y/n)";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "y" ->
      save_to_json account;
      print_endline ""
  | "n" -> ()
  | _ ->
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.red ]
        "⛔ Please enter a correct command ⛔";
      print_endline "";
      print_endline "";
      save account

let quit_save account =
  save account;
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.red ] "Quitting";
  wait_alt 0.2;
  ANSITerminal.print_string [ ANSITerminal.red ] ".";
  wait_alt 0.2;
  ANSITerminal.print_string [ ANSITerminal.red ] ".";
  wait_alt 0.2;
  ANSITerminal.print_string [ ANSITerminal.red ] ".";
  wait_alt 0.2;
  ANSITerminal.print_string [ ANSITerminal.red ] ".";
  print_endline "";
  exit 0

let parse_json file_name =
  Yojson.Basic.from_file (direc_file_prefix ^ file_name ^ ".json")

let rec inFile account =
  print_endline "";
  print_endline "What would you like to do?";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "return" -> inFile account
  | "help" ->
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.blue ] "💭 Commands List: ";
      print_endline "";
      print_endline help_str;
      inFile account
  | "get balance" ->
      wait 0.2;
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.blue ] "💵 Balance: ";
      print_string (string_of_int (Finance.Account.balance account) ^ "$");
      print_endline "";
      inFile account
  | "get owner" ->
      wait 0.2;
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.blue ] "👔 Owner: ";
      print_string (Finance.Account.owner account);
      print_endline "";
      inFile account
  | "get account type" ->
      wait 0.2;
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.blue ] "📋 Account Type: ";
      print_string (Finance.Account.account_type account);
      print_endline "";
      inFile account
  | "get interest rate" ->
      wait 0.2;
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "➖ Account Interest Rate: ";
      print_string
        (string_of_float
           (float_of_int (Finance.Account.account_interest account) *. 0.1)
        ^ "%");
      print_endline "";
      inFile account
  | "get account status" ->
      wait 0.2;
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.blue ] "🧾 Account Status: ";
      print_string (Finance.Account.status account);
      print_endline "";
      inFile account
  | "get limit" ->
      wait 0.2;
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.blue ] "🛑 Account Limit: ";
      print_string (string_of_int (Finance.Account.limit account) ^ "$");
      print_endline "";
      inFile account
  | "get max" ->
      wait 0.2;
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.blue ] "⏸️  Account Maximum: ";
      print_string (string_of_int (Finance.Account.maximum account) ^ "$");
      print_endline "";
      inFile account
  | "withdraw" ->
      wait 0.2;
      print_endline "";
      withdraw account
  | "deposit" ->
      wait 0.2;
      print_endline "";
      deposit account
  | "activate account" ->
      wait 0.2;
      print_endline "";
      activate_acct account
  | "deactivate account" ->
      wait 0.2;
      print_endline "";
      deactivate_acct account
  | "get stocks value" ->
      wait 0.2;
      print_endline "";
      get_stock_value account
  | "start transfer" ->
      wait 0.2;
      print_endline "";
      transfer account
  | "get projected balance" ->
      wait 0.2;
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "🏦 Projected Account Balance: ";
      print_string
        (string_of_int (Finance.Account.yearly_projected_balance account) ^ "$");
      print_endline "";
      print_endline
        "^Projected amount after one year based on your current interest rate \
         and account finances";
      print_endline "";
      inFile account
  | "mortgage check" ->
      wait 0.2;
      print_endline "";
      get_mortgage account
  | "get properties" ->
      wait 0.2;
      print_endline "";
      get_property_info account
  | "remove properties" ->
      wait 0.2;
      print_endline "";
      remove_property account
  | "get last transaction" ->
      wait 0.2;
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.blue ] "🏦 Latest Transaction: ";
      print_string (Finance.Account.latest_transaction account ^ "$");
      print_endline "";
      inFile account
  | "get transactions" ->
      wait 0.2;
      print_endline "";
      get_transactions account
  | "set property rent" ->
      wait 0.2;
      print_endline "";
      set_rent account
  | "set property expenses" ->
      wait 0.2;
      print_endline "";
      set_prop_expenses account
  | "quit" -> quit_save account
  | _ ->
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.red ]
        " ⛔ Please enter a correct command ⛔ ";
      print_endline "";
      print_endline "";
      inFile account

and withdraw account =
  if Finance.Account.is_active account = false then (
    print_endline "";
    ANSITerminal.print_string [ ANSITerminal.red ]
      " ⛔ This account is not active withdrawals ⛔ ";
    print_endline "";
    inFile account)
  else begin
    print_endline "";
    print_endline "How much would you like to withdraw?";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | "quit" -> quit_save account
    | "return" -> inFile account
    | amount -> (
        try
          let int_amt = int_of_string amount in
          let balance = Finance.Account.balance account in
          if Finance.Account.maximum account < int_amt then (
            print_endline "";
            ANSITerminal.print_string [ ANSITerminal.red ]
              " ⛔ This amount is greater than your account maximum! ⛔ ";
            print_endline "";
            withdraw account)
          else if Finance.Account.limit account > balance - int_amt then (
            print_endline "";
            ANSITerminal.print_string [ ANSITerminal.red ]
              " ⛔ This withdrawal will put your account over the limit! ⛔ ";
            print_endline "";
            withdraw account)
          else if balance < int_amt then (
            print_endline "";
            ANSITerminal.print_string [ ANSITerminal.red ]
              " ⛔ Insufficient funds! The withdrawal amount is too great ⛔ ";
            print_endline "";
            withdraw account)
          else (
            print_endline "";
            ANSITerminal.print_string [ ANSITerminal.blue ] "Withdrawing ...";
            wait 0.2;
            print_endline "";
            Finance.Account.withdraw account int_amt;
            ANSITerminal.print_string [ ANSITerminal.green ] "💰 Withdrew ";
            print_string (amount ^ "$");
            print_endline "";
            inFile account)
        with Failure _ ->
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.red ]
            " ⛔ Please enter an integer value ⛔ ";
          print_endline "";
          withdraw account)
  end

and deposit account =
  if Finance.Account.is_active account = false then (
    print_endline "";
    ANSITerminal.print_string [ ANSITerminal.red ]
      " ⛔ This account is not active for deposits ⛔ ";
    print_endline "";
    inFile account)
  else begin
    print_endline "";
    print_endline "How much would you like to deposit?";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | "return" -> inFile account
    | "quit" -> quit_save account
    | amount -> (
        try
          let int_amt = int_of_string amount in
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.blue ] "Depositing ...";
          wait 0.2;
          print_endline "";
          Finance.Account.deposit account int_amt;
          ANSITerminal.print_string [ ANSITerminal.green ] "💰 Deposited ";
          print_string (amount ^ "$");
          print_endline "";
          inFile account
        with Failure _ ->
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.red ]
            " ⛔ Please enter an integer value ⛔ ";
          print_endline "";
          deposit account)
  end

and activate_acct account =
  if Finance.Account.is_active account then (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "This account is already active";
    print_endline "";
    inFile account)
  else (
    Finance.Account.activate account;
    ANSITerminal.print_string [ ANSITerminal.blue ] "Account Activated!";
    print_endline "";
    print_endline
      "This account is now elligble for transactions, transfers as well as \
       stocks and property operations.";
    inFile account)

and deactivate_acct account =
  if Finance.Account.is_active account then (
    Finance.Account.deactivate account;
    ANSITerminal.print_string [ ANSITerminal.blue ] "Account Deactivated!";
    print_endline "";
    print_endline
      "This account can no longer be used for transactions, transfers as well \
       as stocks and property operations.";
    inFile account)
  else (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "This account is already non active";
    print_endline "";
    inFile account)

and get_stock_value account =
  ANSITerminal.print_string [ ANSITerminal.blue ] "Crunching numbers ...";
  print_endline "";
  print_endline "";
  let amount = string_of_int (Finance.Account.stocks_value account) in
  ANSITerminal.print_string [ ANSITerminal.blue ] "📈 Total Stock value: ";
  print_endline (amount ^ "$");
  inFile account

and transfer account =
  if Finance.Account.is_active account = false then (
    print_endline "";
    ANSITerminal.print_string [ ANSITerminal.red ]
      " ⛔ This account is not active for transfers ⛔ ";
    print_endline "";
    inFile account)
  else begin
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "Who would you like to transfer to? Please input their account ID";
    print_endline "";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | "return" -> inFile account
    | "quit" -> quit_save account
    | id2 -> begin
        try
          let id2_int = int_of_string id2 in
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.blue ]
            "How much would you like to send?";
          print_endline "";
          print_string "> ";
          match read_line () with
          | exception End_of_file -> ()
          | "return" -> inFile account
          | "quit" -> quit_save account
          | amount -> begin
              try
                let int_amt = int_of_string amount in
                let balance = Finance.Account.balance account in
                if Finance.Account.maximum account < int_amt then (
                  print_endline "";
                  ANSITerminal.print_string [ ANSITerminal.red ]
                    " ⛔ This amount is greater than your account maximum! ⛔ ";
                  print_endline "";
                  transfer account)
                else if Finance.Account.limit account > balance - int_amt then (
                  print_endline "";
                  ANSITerminal.print_string [ ANSITerminal.red ]
                    " ⛔ This transfer will put your account over the limit! ⛔ ";
                  print_endline "";
                  transfer account)
                else if balance < int_amt then (
                  print_endline "";
                  ANSITerminal.print_string [ ANSITerminal.red ]
                    " ⛔ Insufficient funds! The transfer amount is too great ⛔ ";
                  print_endline "";
                  transfer account)
                else if Finance.Account.is_active id2_int = false then (
                  print_endline "";
                  ANSITerminal.print_string [ ANSITerminal.red ]
                    " ⛔ This account is not actively receiving transfers ⛔ ";
                  print_endline "";
                  inFile account)
                else (
                  print_endline "";
                  ANSITerminal.print_string [ ANSITerminal.blue ]
                    "Transferring ...";
                  wait 0.2;
                  print_endline "";
                  Finance.Account.transfer account id2_int int_amt;
                  ANSITerminal.print_string [ ANSITerminal.green ]
                    "💰 Transferred ";
                  print_string (amount ^ "$");
                  ANSITerminal.print_string [ ANSITerminal.green ]
                    " to account ID: ";
                  print_string id2;
                  print_endline "";
                  inFile account)
              with Failure _ ->
                print_endline "";
                ANSITerminal.print_string [ ANSITerminal.red ]
                  " ⛔ Please enter an integer value and make sure the account \
                   exists! ⛔ ";
                print_endline "";
                transfer account
            end
        with Failure _ ->
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.red ]
            " ⛔ Please enter an integer ID value ⛔ ";
          print_endline "";
          transfer account
      end
  end

and get_mortgage account =
  if Finance.Account.is_active account = false then (
    print_endline "";
    ANSITerminal.print_string [ ANSITerminal.red ]
      " ⛔ This account is not active for transfers ⛔ ";
    print_endline "";
    inFile account)
  else begin
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "Choose a property ID from the listed properties in your account";
    print_endline "";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | "quit" -> quit_save account
    | "return" -> inFile account
    | str_id -> (
        try
          let prop_id = int_of_string str_id in
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.blue ]
            "What is the mortgage amount?";
          print_endline "";
          print_string "> ";
          match read_line () with
          | exception End_of_file -> ()
          | "return" -> inFile account
          | "quit" -> quit_save account
          | amount_str -> (
              try
                let amount = int_of_string amount_str in
                print_endline "";
                (match Finance.Account.get_mortgage account prop_id amount with
                | "Not Enough Funds for Said Property" ->
                    ANSITerminal.print_string [ ANSITerminal.yellow ]
                      ("Not Approved! not enough funds for property ID "
                     ^ str_id)
                | "Approved Mortgage" ->
                    ANSITerminal.print_string [ ANSITerminal.green ]
                      ("Approved! Based on your funds, a mortgage would be \
                        valid on Property ID " ^ str_id)
                | _ ->
                    print_endline
                      "Error in generating your request. Please try again later");
                print_endline "";
                inFile account
              with Failure _ ->
                print_endline "";
                ANSITerminal.print_string [ ANSITerminal.red ]
                  " ⛔ Please enter an integer ID value ⛔ ";
                print_endline "";
                get_mortgage account)
        with Failure _ ->
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.red ]
            " ⛔ Please enter an integer ID value ⛔ ";
          print_endline "";
          get_mortgage account)
  end

and get_property_info account =
  if Finance.Account.is_active account = false then (
    print_endline "";
    ANSITerminal.print_string [ ANSITerminal.red ]
      " ⛔ This account is not active for transfers ⛔ ";
    print_endline "";
    inFile account)
  else begin
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "Choose a property ID from the listed properties in your account";
    print_endline "";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | "return" -> inFile account
    | "quit" -> quit_save account
    | str_id -> (
        try
          let prop_id = int_of_string str_id in
          let info_list = Finance.Account.get_property_info account prop_id in
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.blue ] "Property ID: ";
          print_string (List.nth info_list 0);
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.blue ] "Remaining Mortgage: ";
          print_string (List.nth info_list 1 ^ "$");
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.blue ]
            "Mortgage Monthly Cost: ";
          print_string (List.nth info_list 2 ^ "$");
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.blue ]
            "Current Rental Income: ";
          print_string (List.nth info_list 3 ^ "$");
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.blue ]
            "Current Property Expenses: ";
          print_string (List.nth info_list 4 ^ "$");
          print_endline "";
          print_endline "";
          inFile account
        with Failure _ ->
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.red ]
            " ⛔ Please enter an integer Property ID value that is in your \
             account ⛔ ";
          print_endline "";
          get_property_info account)
  end

and remove_property account =
  if Finance.Account.is_active account = false then (
    print_endline "";
    ANSITerminal.print_string [ ANSITerminal.red ]
      " ⛔ This account is not active for transfers ⛔ ";
    print_endline "";
    inFile account)
  else begin
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "Choose a property ID from the listed properties in your account";
    print_endline "";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | "return" -> inFile account
    | "quit" -> quit_save account
    | str_id -> (
        try
          let prop_id = int_of_string str_id in
          Finance.Account.remove_property account prop_id;
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.yellow ]
            ("Removed property " ^ str_id);
          print_endline "";
          inFile account
        with Failure _ ->
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.red ]
            " ⛔ Please enter an integer ID value ⛔ ";
          print_endline "";
          remove_property account)
  end

and set_rent account =
  if Finance.Account.is_active account = false then (
    print_endline "";
    ANSITerminal.print_string [ ANSITerminal.red ]
      " ⛔ This account is not active for transfers ⛔ ";
    print_endline "";
    inFile account)
  else begin
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "Choose a property ID from the listed properties in your account";
    print_endline "";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | "return" -> inFile account
    | "quit" -> quit_save account
    | str_id -> (
        try
          let prop_id = int_of_string str_id in
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.blue ]
            "What is the rent amount?";
          print_endline "";
          print_string "> ";
          match read_line () with
          | exception End_of_file -> ()
          | "return" -> inFile account
          | "quit" -> quit_save account
          | amount_str -> (
              try
                let amount = int_of_string amount_str in
                print_endline "";
                Finance.Account.set_rent account prop_id amount;
                ANSITerminal.print_string [ ANSITerminal.green ]
                  "The rent for property ID ";
                print_string str_id;
                ANSITerminal.print_string [ ANSITerminal.green ]
                  " has been set to ";
                print_string (amount_str ^ "$");
                print_endline "";
                inFile account
              with Failure _ ->
                print_endline "";
                ANSITerminal.print_string [ ANSITerminal.red ]
                  " ⛔ Error - Please enter an integer Property ID value that \
                   is in your account ⛔ ";
                print_endline "";
                set_rent account)
        with Failure _ ->
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.red ]
            " ⛔ Error - Please enter an integer Property ID value that is in \
             your account ⛔ ";
          print_endline "";
          set_rent account)
  end

and set_prop_expenses account =
  if Finance.Account.is_active account = false then (
    print_endline "";
    ANSITerminal.print_string [ ANSITerminal.red ]
      " ⛔ This account is not active for transfers ⛔ ";
    print_endline "";
    inFile account)
  else begin
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "Choose a property ID from the listed properties in your account";
    print_endline "";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | "return" -> inFile account
    | "quit" -> quit_save account
    | str_id -> (
        try
          let prop_id = int_of_string str_id in
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.blue ]
            "What is the expenses amount?";
          print_endline "";
          print_string "> ";
          match read_line () with
          | exception End_of_file -> ()
          | "return" -> inFile account
          | "quit" -> quit_save account
          | amount_str -> (
              try
                let amount = int_of_string amount_str in
                print_endline "";
                Finance.Account.set_hoa_upkeep_and_other_expenses account
                  prop_id amount;
                ANSITerminal.print_string [ ANSITerminal.green ]
                  "The expenses for property ID ";
                print_string str_id;
                ANSITerminal.print_string [ ANSITerminal.green ]
                  " has been set to ";
                print_string (amount_str ^ "$");
                print_endline "";
                inFile account
              with Failure _ ->
                print_endline "";
                ANSITerminal.print_string [ ANSITerminal.red ]
                  " ⛔ Error - Please enter an integer Property ID value that \
                   is in your account ⛔ ";
                print_endline "";
                set_prop_expenses account)
        with Failure _ ->
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.red ]
            " ⛔ Error - Please enter an integer Property ID value that is in \
             your account ⛔ ";
          print_endline "";
          set_prop_expenses account)
  end

and get_transactions account =
  print_endline "";
  let history = Finance.Account.all_transactions account in
  let rec print_transactions lst =
    match lst with
    | [] -> print_endline ""
    | h :: t ->
        print_endline (h ^ "$" ^ "   ");
        print_transactions t
  in
  print_transactions history;
  inFile account

let rec accessFile file_name =
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.blue ] "Accesssing account: ";
  print_string file_name;
  print_endline "";
  wait 1.0;
  ANSITerminal.print_string [ ANSITerminal.blue ] "Querying information ...";
  print_endline "";
  wait 1.0;
  ANSITerminal.print_string [ ANSITerminal.blue ] "Setting up system ...";
  print_endline "";
  wait 1.0;
  let file_path = direc_file_prefix ^ file_name ^ ".json" in
  if Sys.file_exists file_path then (
    ANSITerminal.print_string [ ANSITerminal.green ]
      "===============================";
    print_endline "";
    print_endline "";
    ANSITerminal.print_string [ ANSITerminal.green ] "Currently in ";
    print_string (file_name ^ " 📂");
    print_endline "";
    print_endline "";
    ANSITerminal.print_string [ ANSITerminal.green ]
      "===============================";
    print_endline "";
    wait 0.5;
    file_name_ref := direc_file_prefix ^ file_name ^ ".json";
    try
      let account = Finance.Account.from_json (file_name |> parse_json) in
      inFile account
    with Type_error _ ->
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.red ]
        " ⛔ This Account Sheet is malformed! Please adhere to the Account \
         Sheet format  ⛔ ";
      print_endline "";
      start_query ())
  else (
    print_endline "";
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "⛔ This file cannot be found. Please check if file exits and accessible ⛔";
    print_endline "";
    print_endline "";
    print_endline "Please enter the account name: ";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | "quit" -> quit ()
    | file_name -> accessFile file_name)

and create_account () =
  print_endline "";
  print_endline "";
  print_endline "Please enter the account owner";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | owner -> (
      print_endline "";
      print_endline
        "Please enter the account type [ Savings ; Checking ; Credit ]";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | account_type -> (
          print_endline "";
          print_endline "Please enter an account interest rate (0 if none)";
          print_string "> ";
          match read_line () with
          | exception End_of_file -> ()
          | acc_interest_str -> (
              try
                let acc_interest =
                  int_of_float (float_of_string acc_interest_str *. 10.0)
                in
                print_endline "";
                print_endline "Please enter an account balance (0 if none)";
                print_string "> ";
                match read_line () with
                | exception End_of_file -> ()
                | acc_balance_str -> (
                    try
                      let acc_balance = int_of_string acc_balance_str in
                      print_endline "";
                      print_endline "Please enter an account limit (0 if none)";
                      print_string "> ";
                      match read_line () with
                      | exception End_of_file -> ()
                      | acc_limit_str -> (
                          try
                            let acc_limit = int_of_string acc_limit_str in
                            print_endline "";
                            print_endline
                              "Please enter an account maximum (0 if none)";
                            print_string "> ";
                            match read_line () with
                            | exception End_of_file -> ()
                            | acc_max_str -> (
                                try
                                  let acc_max = int_of_string acc_max_str in
                                  print_endline "";
                                  ANSITerminal.print_string
                                    [ ANSITerminal.blue ] "Creating account ...";
                                  print_endline "";
                                  wait 1.0;
                                  ANSITerminal.print_string
                                    [ ANSITerminal.blue ]
                                    "Querying information ...";
                                  print_endline "";
                                  wait 1.0;
                                  ANSITerminal.print_string
                                    [ ANSITerminal.blue ]
                                    "Setting up system ...";
                                  print_endline "";
                                  wait 1.0;
                                  let new_account =
                                    Finance.Account.create_account owner
                                      account_type acc_interest acc_balance
                                      acc_limit acc_max
                                  in
                                  ANSITerminal.print_string
                                    [ ANSITerminal.green ]
                                    "Successfully created account! Welcome ";
                                  print_endline owner;
                                  inFile new_account
                                with Failure _ ->
                                  print_endline "";
                                  ANSITerminal.print_string [ ANSITerminal.red ]
                                    " ⛔ Please enter an integer value ⛔ ";
                                  print_endline "";
                                  create_account ())
                          with Failure _ ->
                            print_endline "";
                            ANSITerminal.print_string [ ANSITerminal.red ]
                              " ⛔ Please enter an integer value ⛔ ";
                            print_endline "";
                            create_account ())
                    with Failure _ ->
                      print_endline "";
                      ANSITerminal.print_string [ ANSITerminal.red ]
                        " ⛔ Please enter an integer value ⛔ ";
                      print_endline "";
                      create_account ())
              with Failure _ ->
                print_endline "";
                ANSITerminal.print_string [ ANSITerminal.red ]
                  " ⛔ Please enter an integer value ⛔ ";
                print_endline "";
                create_account ())))

and start_query () =
  wait 0.3;
  print_endline "Would you like to access an existing account 🧾 ? (y/n)";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "y" -> (
      wait_alt 0.3;
      print_endline "";
      print_endline "Please enter the account name: ";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | "quit" -> quit ()
      | file_name -> accessFile file_name)
  | "n" -> (
      wait_alt 0.3;
      print_endline "";
      print_endline "";
      print_endline "Would you like to make an account 🔨 ? (y/n)";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | "y" -> create_account ()
      | "n" ->
          print_endline "";
          start_query ()
      | "quit" -> quit ()
      | _ ->
          print_endline "";
          ANSITerminal.print_string [ ANSITerminal.red ]
            " ⛔ Please enter a correct command ⛔ ";
          print_endline "";
          print_endline "";
          start_query ())
  | "quit" -> quit ()
  | _ ->
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.red ]
        " ⛔ Please enter a correct command ⛔ ";
      print_endline "";
      print_endline "";
      start_query ()

let main () =
  print_endline "";
  print_endline "";
  ANSITerminal.print_string
    [ ANSITerminal.green; ANSITerminal.Bold ]
    "Welcome to Juice 🧃. An interactive Finance budgetting engine that \
     organizes your money 💰, provides insights on your portfolio 🔍, and allows \
     you to plan and manage your accounts 🗂️!";
  print_endline "";
  start_query ()

let () = main ()
