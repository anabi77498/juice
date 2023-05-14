(** Representation of static account information.

    This module represents the data stored in financial accounts, including the
    account balance, limit, and assets. It handles loading of that data from
    JSON as well as querying the data.

    For examples, the specifications in this interface reference the example
    "Sample" adventure found in [data/Sample.json]. *)

exception LimitExceeded of int
exception MaximumExceeded of int
exception InactiveAccount
exception InsufficientFunds

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

type property = {
  id : int;
  remaining_mortgage : int;
  mortgage_monthly_cost : int;
  current_rental_income : int;
  hoa_upkeep_and_other_expenses : int;
}

val from_json : Yojson.Basic.t -> int
(** [from_json j] is the account that [j] represents. Requires: [j] is a valid
    JSON account representation. *)

val get_acc : int -> t
(* [get_acc i] retrieves the account with id [i]. *)

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
(** [yearly_projected_balance i] takes in the [i] which is the id of an account
    returns the amount the account would have after a year solely based on
    interest. *)

val approved_mortgage : int -> int -> int -> string
(** [approved_mortgage i prop_id mortgage_value] takes in the [i] which is the
    id of an account, [prop_id] which is the id of the newly approved property
    and [mortgage_value] which is the amount of the mortgage which is being
    taken it out. After updating the account with the new property it returns a
    string stating that the mortgage was approved. This is a helper function and
    is only called when approved by get_mortgage (meaning there are sufficient
    funds in account holders account for the mortgage). *)

val get_mortgage : int -> int -> int -> string
(** [get_mortgage i prop_id property_value] takes in the [i] which is the id of
    an account, [prop_id] which is the desired id of the property (no other
    property of the same user can have the same id), and [property_value] which
    is the total cost of the desired property. If the mortgage can be afforded
    then approved_mortgage will be called. Otherwise, a string stating there are
    not enough funds will be returned. RI: The user does not have another
    property with the same id *)

val identify_property_helper : property list -> int -> property
(** [identify_property_helper properties prop_id] takes in the [properties]
    which is a list of all the properties owned by the user. It also takes in
    the [prop_id] of the property. It will return the property with that prop_id
    or it fails if the owner has no property with that id. *)

val identify_property : int -> int -> property
(** [identify_property i prop_id] takes in the [i] which is the id of the user
    and the [prop_id] of the property and returns the user's property with that
    id. Uses identify_property_helper. *)

val remove_property_helper :
  property list -> int -> property list -> property list
(** [remove_property_helper properties prop_id
    property list] takes in the
    [properties] which is the properties of an account and filters through them.
    If they do match the prop_id the property will be removed. If not it will be
    added to the accumulator and returned. *)

val remove_property : int -> int -> unit
(** [remove_property i prop_id] takes in the [i] which is the id of the user and
    the [prop_id] of the property you want to remove. It will then return the
    account with that property removed *)

val latest_transaction : int -> string
val all_transactions : int -> string list

val set_rent : int -> int -> int -> unit
(** [set_rent i prop_id rent] takes in the [i] which is the id of the user and
    the [prop_id] of the property you want to update. It will then set the rent
    value of that property to [rent] return the account with that property's
    rent updated *)

val set_hoa_upkeep_and_other_expenses : int -> int -> int -> unit
(** [set_hoa_upkeep_and_other_expenses i prop_id hoa_etc] takes in the [i] which
    is the id of the user and the [prop_id] of the property you want to update.
    It will then set the hoa_upkeep_and_other_expenses value of that property to
    [hoa_etc] return the account with that property's
    hoa_upkeep_and_other_expenses updated *)

val transactions_value : int -> int
