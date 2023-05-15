(* TODO: - make keywords to access functions (quit, access) - Access a JSON
   file/ create a JSON file *)

(* Utilize Lwt (open) *)
open Yojson.Basic.Util
open Yojson.Basic
open Lwt

let direc_file_prefix = "data" ^ Filename.dir_sep
let file_name_ref : string ref = ref ""
let curr_state : Finance.State.t_state option ref = ref None

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

let load_state account =
  curr_state :=
    Some
      {
        id = account;
        owner = Finance.Account.owner account;
        account_type = Finance.Account.account_type account;
        account_interest = Finance.Account.account_interest account;
        status = Finance.Account.status account;
        balance = Finance.Account.balance account;
        limit = Finance.Account.limit account;
        maximum = Finance.Account.maximum account;
        stocks = [];
        properties = [];
        history = [];
      }

let get_state () =
  match !curr_state with
  | Some x -> x
  | _ -> failwith "no active state"

let create_account () =
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "Sorry, this feature isn't available at the moment but will be soon!";
  quit ()

let rec inFile account =
  (match !curr_state with
  | None -> load_state account
  | Some _ -> ());
  print_endline "";
  print_endline "What would you like to do?";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
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
  | "get stock value" ->
      wait 0.2;
      print_endline "";
      get_stock_value account
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
          withdraw account)
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
  ANSITerminal.print_string [ ANSITerminal.blue ] "📈 Stock value: ";
  print_endline (amount ^ "$");
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
