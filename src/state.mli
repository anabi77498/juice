type t
(** The abstract type of values representing the game state. *)

val create_account : string -> string -> int -> int -> int -> Account.t
(** create_account creates an account *)
