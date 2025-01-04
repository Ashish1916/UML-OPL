type aexp =
  | Int of int (* Constructor for n *)
  | Add of aexp * aexp (* Constructor for a_1 + a_2 *)
  | Mul of aexp * aexp (* Constructor for a_1 * a_2 *)

exception NoRuleApplies
let rec lstep_aexp a =
  match a with
  | Int n -> Int n  (* Already a value *)
  | Add (a1, a2) ->
      (match (lstep_aexp a1, lstep_aexp a2) with
       | (Int n1, Int n2) -> Int (n1 + n2)
       | _ -> raise NoRuleApplies)
  | Mul (a1, a2) ->
      (match (lstep_aexp a1, lstep_aexp a2) with
       | (Int n1, Int n2) -> Int (n1 * n2)
       | _ -> raise NoRuleApplies)
type bexp =
  | True (* Constructor for true *)
  | False (* Constructor for false *)
  | Eq of aexp * aexp (* Constructor for a = a *)
  | Neq of aexp * aexp (* Constructor for a != a *)
  | Leq of aexp * aexp (* Constructor for a <= a *)
  | Gt of aexp * aexp (* Constructor for a > a *)
  | Neg of bexp (* Constructor for !a *)
  | And of bexp * bexp (* Constructor for a && a *)
let rec lstep_bexp b =
  match b with
  | True -> True
  | False -> False
  | Eq (a1, a2) ->
      (match (lstep_aexp a1, lstep_aexp a2) with
       | (Int n1, Int n2) -> if n1 = n2 then True else False
       | _ -> raise NoRuleApplies)
  | Neq (a1, a2) ->
      (match (lstep_aexp a1, lstep_aexp a2) with
       | (Int n1, Int n2) -> if n1 <> n2 then True else False
       | _ -> raise NoRuleApplies)
  | Leq (a1, a2) ->
      (match (lstep_aexp a1, lstep_aexp a2) with
       | (Int n1, Int n2) -> if n1 <= n2 then True else False
       | _ -> raise NoRuleApplies)
  | Gt (a1, a2) ->
      (match (lstep_aexp a1, lstep_aexp a2) with
       | (Int n1, Int n2) -> if n1 > n2 then True else False
       | _ -> raise NoRuleApplies)
  | Neg b1 ->
      (match lstep_bexp b1 with
       | True -> False
       | False -> True
       | _ -> raise NoRuleApplies)
  | And (b1, b2) ->
      (match lstep_bexp b1 with
       | True -> lstep_bexp b2
       | False -> False
       | _ -> raise NoRuleApplies)