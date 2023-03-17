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
  | Frozen

type stock = {
  shares : int;
  current_value : int;
  api_access : string;
}

type transaction = {
  transaction_type : string;
  amount : int;
}

type t = {
  owner : string;
  account_type : account;
  status : status;
  balance : int;
  limit : int;
  maximum : int;
  stocks : stock list;
  history : transaction list;
}

let stock_of_json lst = []

let make_transaction = function
  | x ->
      {
        transaction_type = (if x > 0 then "Withdrawal" else "Deposit");
        amount = x;
      }

let transaction_of_json lst = List.map make_transaction lst

let parse_acc_type = function
  | "Savings" -> Savings
  | "Checking" -> Checking
  | "Credit" -> Credit
  | _ -> failwith "This should never happen, given from_json's precondition"

let parse_stat_type = function
  | "Active" -> Active
  | "Inactive" -> Inactive
  | "Frozen" -> Frozen
  | _ -> failwith "This should never happen, given from_json's precondition"

let from_json json =
  let open Yojson.Basic.Util in
  {
    owner = json |> to_assoc |> List.assoc "owner" |> to_string;
    account_type =
      json |> to_assoc |> List.assoc "account_type" |> to_string
      |> parse_acc_type;
    status =
      json |> to_assoc |> List.assoc "status" |> to_string |> parse_stat_type;
    balance = json |> to_assoc |> List.assoc "balance" |> to_int;
    limit = json |> to_assoc |> List.assoc "limit" |> to_int;
    maximum = json |> to_assoc |> List.assoc "maximum" |> to_int;
    stocks =
      [] (*json |> member "stocks" |> to_list |> List.map stock_of_json;*);
    history =
      []
      (*json |> member "history" |> to_list |> List.map transaction_of_json;*);
  }

let create_account owner acc_type balance limit maximum =
  assert (limit >= 0);
  {
    owner;
    account_type = acc_type |> parse_acc_type;
    status = Active;
    balance;
    limit;
    maximum;
    stocks = [];
    history = [];
  }

let owner acc = acc.owner
let account_type acc = acc.account_type
let status acc = acc.status
let balance acc = acc.balance
let limit acc = acc.limit
let maximum acc = acc.maximum

let rec stocks_value = function
  | [] -> 0
  | h :: t -> h.current_value + stocks_value t

let withdraw acc n =
  if acc.status <> Active then raise InactiveAccount
  else if acc.balance - n < acc.limit then raise (LimitExceeded n)
  else if n <= acc.maximum then
    {
      acc with
      balance = acc.balance - n;
      history = { transaction_type = "Withdrawal"; amount = n } :: acc.history;
    }
  else raise (MaximumExceeded n)

let deposit acc n =
  if acc.status <> Active then raise InactiveAccount
  else
    {
      acc with
      balance = acc.balance + n;
      history = { transaction_type = "Deposit"; amount = n } :: acc.history;
    }

let close acc = { acc with status = Frozen }

(*let latest_transaction acc = List.head acc.history*)
