(* TODO: - make keywords to access functions (quit, access) - Access a JSON
   file/ create a JSON file *)

(* Utilize Lwt (open) *)
open Yojson.Basic.Util
open Lwt

let direc_file_prefix = "data" ^ Filename.dir_sep

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

let save_to_json () =
  ANSITerminal.print_string [ ANSITerminal.green ] "Saving......";
  print_endline "";
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "Sorry, this feature isn't available at the moment but will be soon!"

let rec save () =
  print_endline "";
  print_endline "Would you like to save your changes? (y/n)";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "y" ->
      save_to_json ();
      print_endline "" (* TODO: IMPLEMENT SAVING TO A JSON FUNCTION *)
  | "n" -> ()
  | _ ->
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.red ]
        "⛔ Please enter a correct command ⛔";
      print_endline "";
      print_endline "";
      save ()

let quit_save () =
  save ();
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

let parse_json file_name =
  Yojson.Basic.from_file (direc_file_prefix ^ file_name ^ ".json")

let create_account () =
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "Sorry, this feature isn't available at the moment but will be soon!";
  quit ()

let rec inFile file_name =
  let account = Finance.Account.from_json (file_name |> parse_json) in
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
      inFile file_name
  | "get owner" ->
      wait 0.2;
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.blue ] "👔 Owner: ";
      print_string (Finance.Account.owner account);
      print_endline "";
      inFile file_name
  | "get account type" ->
      wait 0.2;
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.blue ] "📋 Account Type: ";
      print_string (Finance.Account.account_type account);
      print_endline "";
      inFile file_name
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
      inFile file_name
  | "get limit" ->
      wait 0.2;
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.blue ] "🛑 Account Limit: ";
      print_string (string_of_int (Finance.Account.limit account) ^ "$");
      print_endline "";
      inFile file_name
  | "get max" ->
      wait 0.2;
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.blue ] "⏸️  Account Maximum: ";
      print_string (string_of_int (Finance.Account.maximum account) ^ "$");
      print_endline "";
      inFile file_name
  | "quit" -> quit_save ()
  | _ ->
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.red ]
        " ⛔ Please enter a correct command ⛔ ";
      print_endline "";
      print_endline "";
      inFile file_name

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
    inFile file_name)
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

let rec start_query () =
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
