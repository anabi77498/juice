exception LimitExceeded of int
exception MaximumExceeded of int
exception InactiveAccount

type account =
  | Savings
  | Checking
  | Credit

type status =
  | Active
  | Inactive

type stock = {
  shares : int;
  cur_value_share : int;
  api_access : string;
}

type transaction = {
  transaction_type : string;
  amount : int;
}

type property = {
  remaining_mortgage : int;
  mortgage_monthly_cost : int;
  current_rental_income : int;
  hoa_upkeep_and_other_expenses : int;
}

type t = {
  id : int;
  owner : string;
  account_type : account;
  account_interest : int;
  status : status;
  balance : int;
  limit : int;
  maximum : int;
  stocks : stock list;
  properties : property list;
  history : transaction list;
}

let all_accounts : t list ref = ref []

let rec stock_of_json j =
  let open Yojson.Basic.Util in
  {
    shares = j |> member "Shares" |> to_int;
    cur_value_share = j |> member "Current Value Share" |> to_int;
    api_access = j |> member "API Access" |> to_string;
  }

let rec property_of_json j =
  let open Yojson.Basic.Util in
  {
    remaining_mortgage = j |> member "Remaining Mortgage" |> to_int;
    mortgage_monthly_cost = j |> member "Mortgage Monthly Cost" |> to_int;
    current_rental_income = j |> member "Current Rental Income" |> to_int;
    hoa_upkeep_and_other_expenses =
      j |> member "Hoa, Upkeep, and other Expenses" |> to_int;
  }

let transaction_of_json j =
  let open Yojson.Basic.Util in
  let amt = j |> member "amount" |> to_int in
  {
    transaction_type = (if amt > 0 then "Withdrawal" else "Deposit");
    amount = amt;
  }

let parse_acc_type = function
  | "Savings" -> Savings
  | "Checking" -> Checking
  | "Credit" -> Credit
  | _ -> failwith "This should never happen, given from_json's precondition"

let parse_stat_type = function
  | "Active" -> Active
  | "Inactive" -> Inactive
  | _ -> failwith "This should never happen, given from_json's precondition"

let from_json json =
  let open Yojson.Basic.Util in
  let new_acc =
    {
      id = List.length !all_accounts;
      owner = json |> to_assoc |> List.assoc "owner" |> to_string;
      account_type =
        json |> to_assoc |> List.assoc "account_type" |> to_string
        |> parse_acc_type;
      account_interest =
        json |> to_assoc |> List.assoc "account_interest" |> to_int;
      status =
        json |> to_assoc |> List.assoc "status" |> to_string |> parse_stat_type;
      balance = json |> to_assoc |> List.assoc "balance" |> to_int;
      limit = json |> to_assoc |> List.assoc "limit" |> to_int;
      maximum = json |> to_assoc |> List.assoc "maximum" |> to_int;
      stocks = json |> member "stocks" |> to_list |> List.map stock_of_json;
      properties =
        json |> member "properties" |> to_list |> List.map property_of_json;
      history =
        json |> member "history" |> to_list |> List.map transaction_of_json;
    }
  in
  all_accounts := new_acc :: !all_accounts;
  new_acc.id

let create_account owner acc_type interest balance limit maximum =
  assert (limit >= 0);
  let new_acc =
    {
      id = List.length !all_accounts;
      owner;
      account_type = acc_type |> parse_acc_type;
      account_interest = interest;
      status = Active;
      balance;
      limit;
      maximum;
      stocks = [];
      properties = [];
      history = [];
    }
  in
  all_accounts := new_acc :: !all_accounts;
  new_acc.id

let get_acc id = List.nth !all_accounts id

let update_all_accounts id new_acc =
  all_accounts :=
    List.mapi (fun i x -> if i = id then new_acc else x) !all_accounts

let owner id =
  let acc = get_acc id in
  acc.owner

let account_type id =
  let acc = get_acc id in
  acc.account_type

let status id =
  let acc = get_acc id in
  acc.status

let balance id =
  let acc = get_acc id in
  acc.balance

let limit id =
  let acc = get_acc id in
  acc.limit

let maximum id =
  let acc = get_acc id in
  acc.maximum

let stocks id =
  (*when writing interface, do not include this function, its not available to
    the user*)
  let acc = get_acc id in
  acc.stocks

let stocks_value id =
  let rec helper lst =
    match lst with
    | [] -> 0
    | h :: t -> (h.cur_value_share * h.shares) + helper t
  in
  id |> stocks |> helper

let withdraw id n =
  let acc = get_acc id in
  let new_acc =
    if acc.status <> Active then raise InactiveAccount
    else if acc.balance - n < acc.limit then raise (LimitExceeded n)
    else if n <= acc.maximum then
      {
        acc with
        balance = acc.balance - n;
        history = { transaction_type = "Withdrawal"; amount = n } :: acc.history;
      }
    else raise (MaximumExceeded n)
  in
  update_all_accounts id new_acc

let deposit id n =
  let acc = get_acc id in
  let new_acc =
    if acc.status <> Active then raise InactiveAccount
    else
      {
        acc with
        balance = acc.balance + n;
        history = { transaction_type = "Deposit"; amount = n } :: acc.history;
      }
  in
  update_all_accounts id new_acc

let activate id =
  let acc = get_acc id in
  let new_acc = { acc with status = Active } in
  update_all_accounts id new_acc

let deactivate id =
  let acc = get_acc id in
  let new_acc = { acc with status = Inactive } in
  update_all_accounts id new_acc

let transfer id1 id2 n =
  let acc1 = get_acc id1 in
  let acc2 = get_acc id2 in
  let new_acc1 =
    {
      acc1 with
      balance = acc1.balance - n;
      history =
        { transaction_type = "Transfer Send"; amount = n } :: acc1.history;
    }
  in
  let new_acc2 =
    {
      acc2 with
      balance = acc2.balance + n;
      history =
        { transaction_type = "Transfer Recieve"; amount = n } :: acc2.history;
    }
  in
  update_all_accounts id1 new_acc1;
  update_all_accounts id2 new_acc2

(*let latest_transaction id = let acc = get_acc id in List.head acc.history*)

let yearly_projected_balance id =
  let acc = get_acc id in
  acc.balance * (10000 + acc.account_interest) / 10000

let approved_mortgage id mortgage_value =
  withdraw id (mortgage_value / 4);
  let acc = get_acc id in
  let new_acc =
    {
      acc with
      properties =
        {
          remaining_mortgage = mortgage_value;
          mortgage_monthly_cost = mortgage_value / 12 / 30;
          current_rental_income = 0;
          hoa_upkeep_and_other_expenses = 0;
        }
        :: acc.properties;
    }
  in
  update_all_accounts id new_acc;
  let msg = "Approved Mortgage" in
  msg

let get_mortgage id property_value =
  let acc = get_acc id in
  if acc.balance > property_value / 5 then
    approved_mortgage id (property_value / 5 * 4)
  else "Not Enough Funds for Said Property"
