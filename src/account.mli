(** Representation of static account information.

    This module represents the data stored in financial accounts, including the
    account balance, limit, and assets. It handles loading of that data from
    JSON as well as querying the data.

    For examples, the specifications in this interface reference the example
    "Sample" adventure found in [data/Sample.json]. *)

type t
(** The abstract type of values representing accounts. *)

type account
(** The abstract type used to represent the type of account. *)

type status
(** The abstract type used to represent the status of account. *)

type stock
(** The abstract type used to represent the stocks of account. *)

type transaction
(** The abstract type used to represent the transactions of account. *)

val from_json : Yojson.Basic.t -> int
(** [from_json j] is the account that [j] represents. Requires: [j] is a valid
    JSON account representation. *)

val owner : int -> string
(** [owner a] is the identifier of the owner in account [a]. Example: the
    [account] of Sample is ["Johnny"]. *)

val balance : t -> int
(* [balance acc] represents the balance of an account *)

(*val assets : t -> string list*)
(** [assets a] is a set-like list of all assets names from the account [a].
    Example: in Sample, the [assets] are ["stock 2"], ["stock 5"], and
    ["stock 6"]. *)

val create_account : string -> string -> int -> int -> int -> int -> int
(** [create_account] allows the user to create an account with a respective
    [owner acc_type balance limit maximum] and returns an id value. [interest]
    is the interest rate multiplied by 100. So a rate of 8.32% is 832 [balance]
    is the amount of money multiplied by 100. So a balance of $1,493.33 is
    149333 *)

val account_type : int -> string
(** [account_type id] takes in an [id] and returns an account. *)

val status : int -> string
(** [status id] takes in an [id] and returns the status of the respective
    account. *)

val account_interest : int -> int
(** [account_interest id] takes in an [id] and returns the interest rate of the
    respective account. *)

val balance : int -> int
(** [balance id] takes in an [id] and returns the balance of the respective
    account. *)

val limit : int -> int
(** [limit id] takes in an [id] and returns the limit of the respective account. *)

val maximum : int -> int
(** [maximum id] takes in an [id] and returns the maximum of the respective
    account. *)

val stocks_value : int -> int
(** [stocks_value id] takes in an [id] and returns the value of the stocks in
    the respective account. *)

val withdraw : int -> int -> unit
(** [withdraw id n] takes in an [id] and from the respective account withdraws
    [n]. *)

val deposit : int -> int -> unit
(** [withdraw id n] takes in an [id] and from the respective account deposits
    [n]. *)

val activate : int -> unit
(** [activate id] takes in an [id] and sets the status of the respective account
    to Active. *)

val deactivate : int -> unit
(** [activate id] takes in an [id] and sets the status of the respective account
    to Inactive. *)

val transfer : int -> int -> int -> unit
(** [transfer id1 id2 n] takes in the [id] of two accounts and transfers [n]
    money between those two accounts. *)

val yearly_projected_balance : int -> int
