open Yojson.Basic

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
