(* TODO: - make keywords (quit, access) - Access a JSON file/ create a JSON
   file *)

(* Utilize LWT (open) *)

let direc_file_prefix = "data" ^ Filename.dir_sep

let rec accessFile file_name =
  ANSITerminal.print_string [ ANSITerminal.blue ] "\nAccesssing account: ";
  print_string (file_name ^ "\n");
  ANSITerminal.print_string [ ANSITerminal.blue ] "Querying information ...\n";
  ANSITerminal.print_string [ ANSITerminal.blue ] "Setting up system ...\n";
  let file_path = direc_file_prefix ^ file_name ^ ".json" in
  if Sys.file_exists file_path then (
    ANSITerminal.print_string [ ANSITerminal.green ] ("\n" ^ "Currently in ");
    print_string (file_name ^ "\n"))
  else
    ANSITerminal.print_string [ ANSITerminal.red ]
      " ❌ This file cannot be found. Please check if file exits and accessible \
       ❌ \n";
  print_endline "\nPlease enter the account name: ";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> accessFile file_name

let getFile () =
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> accessFile file_name

let rec start_query () =
  ANSITerminal.resize 80 50;
  print_endline "Would you like to access an account 🧾 ? (y/n)";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "y" ->
      print_endline "\nPlease enter the account name: ";
      getFile ()
  | "n" ->
      print_endline "\nWould you like to make an account 🔨?";
      print_string "> "
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\n ❌ Please enter a correct command ❌ \n\n";
      start_query ()

let main () =
  ANSITerminal.print_string
    [ ANSITerminal.green; ANSITerminal.Bold ]
    "\n\
     Welcome to Juice 🧃. An interactive Finance budgetting engine that \
     organizes your money 💰, provides insights on your portfolio 🔍, and allows \
     you to plan and manage your accounts 🗂️!\n\n";
  start_query ()

let () = main ()
