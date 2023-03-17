(** Representation of static account information.

    This module represents the data stored in financial accounts, including the
    account balance, limit, and assets. It handles loading of that data from
    JSON as well as querying the data.

    For examples, the specifications in this interface reference the example
    "Sample" adventure found in [data/Sample.json]. *)

type stock

type t
(** The abstract type of values representing accounts. *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the account that [j] represents. Requires: [j] is a valid
    JSON account representation. *)

val owner : t -> string
(** [owner a] is the identifier of the owner in account [a]. Example: the
    [account] of Sample is ["Johnny"]. *)

(*val assets : t -> string list*)
(** [assets a] is a set-like list of all assets names from the account [a].
    Example: in Sample, the [assets] are ["stock 2"], ["stock 5"], and
    ["stock 6"]. *)
