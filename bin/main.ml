(* TODO: - make keywords (quit, access) - Access a JSON file/ create a JSON
   file *)

let rec start_query () =
  ANSITerminal.resize 80 50;
  print_endline "Would you like to access an account 🧾 ? (y/n)";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "y" ->
      print_endline "\nPlease enter the account name: ";
      print_string "> "
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
