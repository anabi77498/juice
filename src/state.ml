(* Type t holds your current account *)
type t = { current_account_id : int }

(* Create account lets you set your account *)
let create_account o acc_type balance limit maximum =
  let open Account in
  let x = Account.create_account o acc_type balance limit maximum in
  { current_account_id = x }
