(* Note: You may introduce new code anywhere in this file. *)

(* TODO: replace [unit] with a type of your own design. *)
type t = { current_account : Account.t }

type result =
  | Legal of t
  | Illegal
