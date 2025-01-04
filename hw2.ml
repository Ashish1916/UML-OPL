type aexp =
  | Int of int
  | Add of aexp * aexp
  | Mul of aexp * aexp

exception NoRuleApplies

let rec sstep_aexp a =
  match a with
  | Int _ -> raise NoRuleApplies
  | Add (Int n1, Int n2) -> Int (n1 + n2)
  | Add (Int n1, a2) ->
      let a2' = sstep_aexp a2 in
      Add (Int n1, a2')
  | Add (a1, a2) ->
      let a1' = sstep_aexp a1 in
      Add (a1', a2)
  | Mul (Int n1, Int n2) -> Int (n1 * n2)
  | Mul (Int n1, a2) ->
      let a2' = sstep_aexp a2 in
      Mul (Int n1, a2')
  | Mul (a1, a2) ->
      let a1' = sstep_aexp a1 in
      Mul (a1', a2)

type bexp =
  | True
  | False
  | Eq of aexp * aexp
  | Neq of aexp * aexp
  | Leq of aexp * aexp
  | Gt of aexp * aexp
  | Neg of bexp
  | And of bexp * bexp

let rec sstep_bexp b =
  match b with
  | True | False -> raise NoRuleApplies
  | Eq (Int n1, Int n2) -> if n1 = n2 then True else False
  | Eq (Int n, a2) ->
      let a2' = sstep_aexp a2 in
      Eq (Int n, a2')
  | Eq (a1, a2) ->
      let a1' = sstep_aexp a1 in
      Eq (a1', a2)
  | Neq (Int n1, Int n2) -> if n1 <> n2 then True else False
  | Neq (Int n, a2) ->
      let a2' = sstep_aexp a2 in
      Neq (Int n, a2')
  | Neq (a1, a2) ->
      let a1' = sstep_aexp a1 in
      Neq (a1', a2)
  | Leq (Int n1, Int n2) -> if n1 <= n2 then True else False
  | Leq (Int n, a2) ->
      let a2' = sstep_aexp a2 in
      Leq (Int n, a2')
  | Leq (a1, a2) ->
      let a1' = sstep_aexp a1 in
      Leq (a1', a2)
  | Gt (Int n1, Int n2) -> if n1 > n2 then True else False
  | Gt (Int n, a2) ->
      let a2' = sstep_aexp a2 in
      Gt (Int n, a2')
  | Gt (a1, a2) ->
      let a1' = sstep_aexp a1 in
      Gt (a1', a2)
  | Neg True -> False
  | Neg False -> True
  | Neg b' ->
      let b'' = sstep_bexp b' in
      Neg b''
  | And (True, True) -> True
  | And (False, _) -> False
  | And (True, b2) -> b2
  | And (b1, b2) ->
      let b1' = sstep_bexp b1 in
      And (b1', b2)
