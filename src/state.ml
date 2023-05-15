(* Type t holds your current account *)

type stock_state = {
  shares : int;
  cur_value_share : int;
  api_access : string;
}

type transaction_state = {
  transaction_type : string;
  amount : int;
}

type property_state = {
  id : int;
  remaining_mortgage : int;
  mortgage_monthly_cost : int;
  current_rental_income : int;
  hoa_upkeep_and_other_expenses : int;
}

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
