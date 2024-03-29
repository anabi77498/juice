exception LimitExceeded of int
exception MaximumExceeded of int
exception InactiveAccount
exception InsufficientFunds

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
  id : int;
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
    id = j |> member "id" |> to_int;
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
    transaction_type = (if amt < 0 then "Withdrawal" else "Deposit");
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
  assert (new_acc.balance >= new_acc.limit);
  assert (new_acc.balance >= 0);
  assert (new_acc.limit >= 0);
  assert (new_acc.maximum >= 0);
  all_accounts := !all_accounts @ [ new_acc ];
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
  assert (new_acc.balance >= new_acc.limit);
  assert (new_acc.balance >= 0);
  assert (new_acc.limit >= 0);
  assert (new_acc.maximum >= 0);
  all_accounts := !all_accounts @ [ new_acc ];
  new_acc.id

let get_acc i = List.nth !all_accounts i

let update_all_accounts u new_acc =
  all_accounts :=
    List.mapi (fun i x -> if i = u then new_acc else x) !all_accounts

let string_of_account = function
  | Savings -> "Savings"
  | Checking -> "Checking"
  | Credit -> "Credit"

let status_of_account = function
  | Active -> "Active"
  | Inactive -> "Inactive"

let owner i =
  let acc = get_acc i in
  acc.owner

let account_type i =
  let acc = get_acc i in
  string_of_account acc.account_type

let status i =
  let acc = get_acc i in
  status_of_account acc.status

let is_active i =
  let acc = get_acc i in
  acc.status = Active

let account_interest i =
  let acc = get_acc i in
  acc.account_interest

let balance i =
  let acc = get_acc i in
  acc.balance

let limit i =
  let acc = get_acc i in
  acc.limit

let maximum i =
  let acc = get_acc i in
  acc.maximum

let stocks i =
  (*when writing interface, do not include this function, its not available to
    the user*)
  let acc = get_acc i in
  acc.stocks

let properties i =
  (*when writing interface, do not include this function, its not available to
    the user*)
  let acc = get_acc i in
  acc.properties

let transactions i =
  (*when writing interface, do not include this function, its not available to
    the user*)
  let acc = get_acc i in
  acc.history

let stocks_value i =
  let rec helper lst =
    match lst with
    | [] -> 0
    | h :: t -> (h.cur_value_share * h.shares) + helper t
  in
  i |> stocks |> helper

let buy_stock acc_id shares value api_key =
  let acc = get_acc acc_id in
  if acc.status = Active then
    let total_value = shares * value in
    if total_value > acc.balance then raise InsufficientFunds
    else if acc.balance - total_value < acc.limit then
      raise (LimitExceeded total_value)
    else if total_value > acc.maximum then raise (MaximumExceeded total_value)
    else
      let new_stock =
        { shares; cur_value_share = value; api_access = api_key }
      in
      let new_stocks = new_stock :: acc.stocks in
      let new_balance = acc.balance - total_value in
      let transactio_type = "Stock Purchase" in
      let trans =
        { transaction_type = transactio_type; amount = -total_value }
      in
      let new_history = trans :: acc.history in
      let updated_acc =
        {
          acc with
          balance = new_balance;
          stocks = new_stocks;
          history = new_history;
        }
      in
      update_all_accounts acc_id updated_acc
  else raise InactiveAccount

let withdraw i n =
  let acc = get_acc i in
  let new_acc =
    if acc.status <> Active then raise InactiveAccount
    else if n > acc.balance then raise InsufficientFunds
    else if acc.balance - n < acc.limit then raise (LimitExceeded n)
    else if n <= acc.maximum then
      {
        acc with
        balance = acc.balance - n;
        history =
          { transaction_type = "Withdrawal"; amount = -n } :: acc.history;
      }
    else raise (MaximumExceeded n)
  in
  update_all_accounts i new_acc

let deposit i n =
  let acc = get_acc i in
  let new_acc =
    if acc.status <> Active then raise InactiveAccount
    else
      {
        acc with
        balance = acc.balance + n;
        history = { transaction_type = "Deposit"; amount = n } :: acc.history;
      }
  in
  update_all_accounts i new_acc

let activate i =
  let acc = get_acc i in
  let new_acc = { acc with status = Active } in
  update_all_accounts i new_acc

let deactivate i =
  let acc = get_acc i in
  let new_acc = { acc with status = Inactive } in
  update_all_accounts i new_acc

let transfer id1 id2 n =
  let acc1 = get_acc id1 in
  let acc2 = get_acc id2 in
  if acc1.status <> Active then raise InactiveAccount
  else if acc2.status <> Active then raise InactiveAccount
  else if n > acc1.balance then raise InsufficientFunds
  else if acc1.balance - n < acc1.limit then raise (LimitExceeded n)
  else if n > acc1.maximum then raise (MaximumExceeded n)
  else
    let new_acc1 =
      {
        acc1 with
        balance = acc1.balance - n;
        history =
          { transaction_type = "Transfer Send"; amount = -n } :: acc1.history;
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

let string_of_transaction t =
  "Transaction Type: " ^ t.transaction_type ^ ", Amount: "
  ^ string_of_int t.amount

let latest_transaction i =
  let acc = get_acc i in
  match acc.history with
  | [] -> "No transactions made"
  | h :: t -> string_of_transaction h

let all_transactions i =
  let acc = get_acc i in
  let rec lst_of_tran l =
    match l with
    | [] -> []
    | h :: t -> string_of_transaction h :: lst_of_tran t
  in
  lst_of_tran acc.history

let transactions_value i =
  let rec helper lst =
    match lst with
    | [] -> 0
    | h :: t -> h.amount + helper t
  in
  i |> transactions |> helper

let yearly_projected_balance i =
  let acc = get_acc i in
  acc.balance * (10000 + acc.account_interest) / 10000

let approved_mortgage i prop_id mortgage_value =
  withdraw i (mortgage_value / 4);
  let acc = get_acc i in
  let new_acc =
    {
      acc with
      properties =
        {
          id = prop_id;
          remaining_mortgage = mortgage_value;
          mortgage_monthly_cost = mortgage_value / 12 / 30;
          current_rental_income = 0;
          hoa_upkeep_and_other_expenses = 0;
        }
        :: acc.properties;
    }
  in
  update_all_accounts i new_acc;
  let msg = "Approved Mortgage" in
  msg

let get_mortgage i prop_id property_value =
  let acc = get_acc i in
  if acc.balance > property_value / 5 then
    approved_mortgage i prop_id (property_value / 5 * 4)
  else "Not Enough Funds for Said Property"

let rec identify_property_helper (properties : property list) prop_id =
  match properties with
  | [] -> failwith "Property does not exist"
  | h :: t -> if h.id = prop_id then h else identify_property_helper t prop_id

let identify_property i prop_id =
  let acc = get_acc i in
  identify_property_helper acc.properties prop_id

let rec identify_property_helper (properties : property list) prop_id =
  match properties with
  | [] -> failwith "Property does not exist"
  | h :: t -> if h.id = prop_id then h else identify_property_helper t prop_id

let get_property_info i prop_id =
  let prop = identify_property i prop_id in
  [
    string_of_int prop_id;
    string_of_int prop.remaining_mortgage;
    string_of_int prop.mortgage_monthly_cost;
    string_of_int prop.current_rental_income;
    string_of_int prop.hoa_upkeep_and_other_expenses;
  ]

let rec remove_property_helper (properties : property list) prop_id
    (acc : property list) =
  match properties with
  | [] -> List.rev acc
  | h :: t ->
      if h.id = prop_id then remove_property_helper t prop_id acc
      else remove_property_helper t prop_id (h :: acc)

let remove_property i prop_id =
  let acc = get_acc i in
  let new_property_list = remove_property_helper acc.properties prop_id [] in
  let new_acc = { acc with properties = new_property_list } in
  update_all_accounts i new_acc

let set_rent i prop_id rent =
  let prop = identify_property i prop_id in
  remove_property i prop_id;
  let acc = get_acc i in
  let new_acc =
    {
      acc with
      properties =
        {
          id = prop_id;
          remaining_mortgage = prop.remaining_mortgage;
          mortgage_monthly_cost = prop.mortgage_monthly_cost;
          current_rental_income = rent;
          hoa_upkeep_and_other_expenses = prop.hoa_upkeep_and_other_expenses;
        }
        :: acc.properties;
    }
  in
  update_all_accounts i new_acc

let set_hoa_upkeep_and_other_expenses i prop_id hoa_etc =
  let prop = identify_property i prop_id in
  remove_property i prop_id;
  let acc = get_acc i in
  let new_acc =
    {
      acc with
      properties =
        {
          id = prop_id;
          remaining_mortgage = prop.remaining_mortgage;
          mortgage_monthly_cost = prop.mortgage_monthly_cost;
          current_rental_income = prop.current_rental_income;
          hoa_upkeep_and_other_expenses = hoa_etc;
        }
        :: acc.properties;
    }
  in
  update_all_accounts i new_acc

let stock_to_json (acc_id : int) : Yojson.Basic.t =
  let stock_lst = stocks acc_id in
  `List
    (List.map
       (fun x ->
         `Assoc
           [
             ("Shares", `Int x.shares);
             ("Current Value Share", `Int x.cur_value_share);
             ("API Access", `String x.api_access);
           ])
       stock_lst)

let prop_to_json (acc_id : int) : Yojson.Basic.t =
  let prop_lst = properties acc_id in
  `List
    (List.map
       (fun x ->
         let prop : property = x in
         `Assoc
           [
             ("id", `Int x.id);
             ("Remaining Mortgage", `Int prop.remaining_mortgage);
             ("Mortgage Monthly Cost", `Int prop.mortgage_monthly_cost);
             ("Current Rental Income", `Int prop.current_rental_income);
             ( "Hoa, Upkeep, and other Expenses",
               `Int prop.hoa_upkeep_and_other_expenses );
           ])
       prop_lst)

let history_to_json (acc_id : int) : Yojson.Basic.t =
  let hist_lst = transactions acc_id in
  `List (List.map (fun x -> `Assoc [ ("amount", `Int x.amount) ]) hist_lst)
