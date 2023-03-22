(* TODO: - make keywords to access functions (quit, access) - Access a JSON
   file/ create a JSON file *)

(* Utilize Lwt (open) *)
open Yojson.Basic.Util

let direc_file_prefix = "data" ^ Filename.dir_sep

let rec save () =
  print_endline "\nWould you like to save your changes? (y/n)";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "y" ->
      ANSITerminal.print_string [ ANSITerminal.green ] "Saving......\n"
      (* TODO: IMPLEMENT SAVING TO A JSON FUNCTION *)
  | "n" -> ()
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\n ⛔ Please enter a correct command ⛔ \n\n";
      save ()

let quit_save () =
  save ();
  ANSITerminal.print_string [ ANSITerminal.red ] "\nQuitting .......\n";
  exit 0

let quit () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\nQuitting .......\n";
  exit 0

let parse_json file_name =
  Yojson.Basic.from_file (direc_file_prefix ^ file_name ^ ".json")

let rec inFile file_name =
  let account = Finance.Account.from_json (file_name |> parse_json) in
  print_endline "\nWhat would you like to do?";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "get balance" ->
      ANSITerminal.print_string [ ANSITerminal.blue ] "\n💵 Balance: ";
      print_string (string_of_int (Finance.Account.balance account) ^ "$\n");
      inFile file_name
  | "get owner" ->
      ANSITerminal.print_string [ ANSITerminal.blue ] "\n👔 Owner: ";
      print_string (Finance.Account.owner account ^ "\n");
      inFile file_name
  | "quit" -> quit_save ()
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\n ⛔ Please enter a correct command ⛔ \n\n";
      inFile file_name

let rec accessFile file_name =
  ANSITerminal.print_string [ ANSITerminal.blue ] "\nAccesssing account: ";
  print_string (file_name ^ "\n");
  ANSITerminal.print_string [ ANSITerminal.blue ] "Querying information ...\n";
  ANSITerminal.print_string [ ANSITerminal.blue ] "Setting up system ...\n";
  let file_path = direc_file_prefix ^ file_name ^ ".json" in
  if Sys.file_exists file_path then (
    ANSITerminal.print_string [ ANSITerminal.green ]
      "===============================\n";
    ANSITerminal.print_string [ ANSITerminal.green ] ("\n" ^ "Currently in ");
    print_string (file_name ^ " 📂\n\n");
    ANSITerminal.print_string [ ANSITerminal.green ]
      "===============================\n";
    inFile file_name)
  else (
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "\n\
      \ ⛔ This file cannot be found. Please check if file exits and accessible \
       ⛔ \n";
    print_endline "\nPlease enter the account name: ";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | "quit" -> quit ()
    | file_name -> accessFile file_name)

let rec start_query () =
  print_endline "Would you like to access an account 🧾 ? (y/n)";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "y" -> (
      print_endline "\nPlease enter the account name: ";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | file_name -> accessFile file_name)
  | "n" ->
      print_endline "\nWould you like to make an account 🔨?";
      print_string "> "
  | "quit" -> quit ()
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\n ⛔ Please enter a correct command ⛔ \n\n";
      start_query ()

let main () =
  ANSITerminal.print_string
    [ ANSITerminal.green; ANSITerminal.Bold ]
    "\n\n\
     Welcome to Juice 🧃. An interactive Finance budgetting engine that \
     organizes your money 💰, provides insights on your portfolio 🔍, and allows \
     you to plan and manage your accounts 🗂️!\n\n";
  start_query ()

let () = main ()