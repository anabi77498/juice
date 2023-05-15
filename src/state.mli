(** Representation of dynamic account state.

    This module represents the state of an account as it is being used,
    including the account's stocks, the transactions that have been done, and
    properties owned. *)

type stock_state = {
  shares : int;
  cur_value_share : int;
  api_access : string;
}
(** The type of values representing the state of a stock. *)

type transaction_state = {
  transaction_type : string;
  amount : int;
}
(** The type of values representing the state of a transaction. *)

type property_state = {
  id : int;
  remaining_mortgage : int;
  mortgage_monthly_cost : int;
  current_rental_income : int;
  hoa_upkeep_and_other_expenses : int;
}
(** The type of values representing the state of a property. *)

type t_state = {
  id : int;
  owner : string;
  account_type : string;
  account_interest : int;
  status : string;
  balance : int;
  limit : int;
  maximum : int;
  stocks : stock_state list;
  properties : property_state list;
  history : transaction_state list;
}
(** The type of values representing the state of an account. *)
