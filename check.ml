open Ast
open Helper

(* Unification algorithm *)
let rec unify (c : constr) : subst =
  match Constr.elements c with
  | [] -> VarMap.empty
  | (t1, t2) :: rest ->
    if t1 = t2 then unify (Constr.of_list rest)
    else match (t1, t2) with
      | (TVar x, t) | (t, TVar x) ->
        if VarSet.mem x (ftvs t) then
          raise (TypeError "Occurs check failed")
        else
          let s = VarMap.singleton x t in
          VarMap.union (fun _ _ v -> Some v) s (unify (Constr.map (subst_typ s) (Constr.of_list rest)))
      | (TArrow (t1, t2), TArrow (t3, t4)) ->
        unify (Constr.add (t1, t3) (Constr.add (t2, t4) (Constr.of_list rest)))
      | _ -> raise (TypeError "Type mismatch")

(* Type inference *)
let rec check (ctx : context) (e : expr) : typ * constr =
  match e with
  | Var x ->
    (try (VarMap.find x ctx, Constr.empty)
     with Not_found -> raise (TypeError ("Unbound variable: " ^ x)))
  | Abs (x, e1) ->
    let tv = TVar (next_tvar()) in
    let (t, c) = check (VarMap.add x tv ctx) e1 in
    (TArrow (tv, t), c)
  | App (e1, e2) ->
    let (t1, c1) = check ctx e1 in
    let (t2, c2) = check ctx e2 in
    let tv = TVar (next_tvar()) in
    (tv, Constr.union c1 (Constr.union c2 (Constr.singleton (t1, TArrow (t2, tv)))))
  | Let (x, e1, e2) ->
    let (t1, c1) = check ctx e1 in
    let (t2, c2) = check (VarMap.add x t1 ctx) e2 in
    (t2, Constr.union c1 c2)
  | LetRec (f, x, e1, e2) ->
    let tv1 = TVar (next_tvar()) in
    let tv2 = TVar (next_tvar()) in
    let ctx' = VarMap.add f (TArrow (tv1, tv2)) ctx in
    let (t1, c1) = check (VarMap.add x tv1 ctx') e1 in
    let c1' = Constr.add (t1, tv2) c1 in
    let (t2, c2) = check ctx' e2 in
    (t2, Constr.union c1' c2)