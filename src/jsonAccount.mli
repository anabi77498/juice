(** JSON representation of an account

    This module represents the Json equivalent of account.ml. It supports taking
    an existing account and converting it to a Json file

    For examples, the specifications in this interface reference the example
    "Sample" adventure found in [data/Sample.json]. *)

val acc_to_json : int -> Yojson.Basic.t
