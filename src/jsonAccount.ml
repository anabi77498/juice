open Yojson.Basic
(* type json = | Null | Bool of bool | Int of int | Intlit of string | Float of
   float | String of string | Assoc of (string * json) list | List of json
   list *)

type json =
  [ `Assoc of (string * t) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of t list
  | `Null
  | `String of string
  ]

let acc_to_json (acc_id : int) : Yojson.Basic.t =
  `Assoc
    [
      ("owner", `String (Account.owner acc_id));
      ("account_type", `String (Account.account_type acc_id));
      ("account_interest", `Int (Account.account_interest acc_id));
      ("status", `String (Account.status acc_id));
      ("balance", `Int (Account.balance acc_id));
      ("limit", `Int (Account.limit acc_id));
      ("maximum", `Int (Account.maximum acc_id));
      ("stocks", `List []);
      ("properties", `List []);
      ("history", `List []);
    ]
