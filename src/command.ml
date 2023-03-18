(* Note: You may introduce new code anywhere in this file. *)

type object_phrase = string list

type command =
  | Create_Account of object_phrase
  | Quit

exception Empty
exception Malformed

let parse str =
  match String.split_on_char ' ' str |> List.filter (fun x -> x <> "") with
  | [] -> raise Empty
  | h :: t ->
      if h = "quit" then if t <> [] then raise Malformed else Quit
      else if h = "create" then
        if t = [] then raise Malformed else Create_Account t
      else raise Malformed
