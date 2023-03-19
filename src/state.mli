type t
(** The abstract type of values representing the game state. *)

val create_account : string -> string -> int -> int -> int -> t
(** create_account creates an account *)
