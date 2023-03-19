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

type t = {
  id : int;
  owner : string;
  account_type : account;
  status : status;
  balance : int;
  limit : int;
  maximum : int;
  stocks : stock list;
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
      status =
        json |> to_assoc |> List.assoc "status" |> to_string |> parse_stat_type;
      balance = json |> to_assoc |> List.assoc "balance" |> to_int;
      limit = json |> to_assoc |> List.assoc "limit" |> to_int;
      maximum = json |> to_assoc |> List.assoc "maximum" |> to_int;
      stocks = json |> member "stocks" |> to_list |> List.map stock_of_json;
      history =
        json |> member "history" |> to_list |> List.map transaction_of_json;
    }
  in
  all_accounts := new_acc :: !all_accounts;
  new_acc.id

let create_account owner acc_type balance limit maximum =
  assert (limit >= 0);
  let new_acc =
    {
      id = List.length !all_accounts;
      owner;
      account_type = acc_type |> parse_acc_type;
      status = Active;
      balance;
      limit;
      maximum;
      stocks = [];
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
