type account =
  | Savings
  | Checking
  | Credit

type status =
  | Active
  | Inactive
  | Closed

type asset = {
  category : string;
  current_value : int;
}

type transaction = {
  sender : string;
  receiver : string;
  amount : int;
}

type t = {
  owner : string;
  account_type : account;
  status : status;
  balance : int;
  limit : int;
  maximum : int;
  assets : asset list;
  history : transaction list;
}

let asset_of_json = []
let transaction_of_json = []

let parse_acc_type = function
  | "Savings" -> Savings
  | "Checking" -> Checking
  | "Credit" -> Credit
  | _ -> failwith "This should never happen, given from_json's precondition"

let parse_stat_type = function
  | "Active" -> Active
  | "Inactive" -> Inactive
  | "Closed" -> Closed
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
    assets =
      [] (*json |> member "assets" |> to_list |> List.map asset_of_json;*);
    history =
      []
      (*json |> member "history" |> to_list |> List.map transaction_of_json;*);
  }

let create_account owner acc_type balance limit maximum =
  {
    owner;
    account_type = acc_type |> parse_acc_type;
    status = Active;
    balance;
    limit;
    maximum;
    assets = [];
    history = [];
  }

let owner acc = acc.owner
