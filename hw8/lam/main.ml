(*
open Ast

let () = 
  let _ = 
    if Array.length Sys.argv <> 2 then
      (Format.printf "Usage: lam <file>\n";
       exit 0) in 
  let file = open_in (Sys.argv.(1)) in 
  let lexbuf = Lexing.from_channel file in 
  let e = 
    try Parser.exp Lexer.token lexbuf
    with Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Format.printf "Syntax error at line %d\n" pos.Lexing.pos_lnum;
      exit 1 in 

  let _ =
    Format.printf "@[";
    Format.printf "Expression:@\n  @[";
    Pprint.printExp e;
    Format.printf "@]@\n@\n" in 

  let _ = 
    Format.printf "Typechecking the expression...@\n";
    Format.print_flush () in 

  let t,c = Check.check VarMap.empty e in 

  let _ = 
    Format.printf "Initial Type:@\n  @[";
    Pprint.printTyp t;
    Format.printf "@]@\n@\n" in 
  
  let _ = 
    Format.printf "Constraints:@\n  @[";
    let _ = 
      Constr.fold 
        (fun (t1,t2) b -> 
          if b then Format.printf "@\n";
          Pprint.printTyp t1;
          Format.printf "@ :rage: ";
          Pprint.printTyp t2;
          true)
        c false in       
    Format.printf "@]@\n@\n" in 
  
  let s = Check.unify c in 

  let _ = 
    Format.printf "Result:@\n  @[";
    Pprint.printTyp (Helper.apply_subst s t);
    Format.printf "@]@\n@\n" in 
  
  let _ = 
    Format.printf "Evaluating the expression...@\n";
    Format.print_flush () in 

  let v = Eval.eval VarMap.empty e in 

  let _ = 
    Format.printf "Result:@\n  @[";
    Pprint.printVal v;
    Format.printf "@]@\n";
    Format.printf "@]" in 
  ()
*)

open Ast

let enable_let_poly = ref false

let () = 
  let args = Array.to_list Sys.argv in
  let file, is_grade_mode = 
    match args with
    | [_; "-grade"; filename] -> (filename, true)
    | [_; "-grade"; "-letpoly"; filename] -> (enable_let_poly := true; filename, true)
    | [_; filename] -> (filename, false)
    | _ -> 
        Format.printf "Usage: lam [-grade [-letpoly]] <file>\n";
        exit 0
  in

  let file_channel = open_in file in 
  let lexbuf = Lexing.from_channel file_channel in 
  let e = 
    try Parser.exp Lexer.token lexbuf
    with Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Format.printf "Syntax error at line %d\n" pos.Lexing.pos_lnum;
      exit 1
  in 

  let _ =
    Format.printf "@[";
    Format.printf "Expression:@\n  @[";
    Pprint.printExp e;
    Format.printf "@]@\n@\n"
  in 

  let _ = 
    Format.printf "Typechecking the expression...@\n";
    Format.print_flush ()
  in 

  let t, c = Check.check VarMap.empty e in 

  let _ = 
    Format.printf "Initial Type:@\n  @[";
    Pprint.printTyp t;
    Format.printf "@]@\n@\n"
  in 
  
  let _ = 
    Format.printf "Constraints:@\n  @[";
    let _ = 
      Constr.fold 
        (fun (t1,t2) b -> 
          if b then Format.printf "@\n";
          Pprint.printTyp t1;
          Format.printf "@ =@ ";
          Pprint.printTyp t2;
          true)
        c false 
    in       
    Format.printf "@]@\n@\n"
  in 
  
  let s = Check.unify c in 

  let final_type = Helper.apply_subst s t in
  let _ = 
    Format.printf "Result:@\n  @[";
    Pprint.printTyp final_type;
    Format.printf "@]@\n@\n"
  in 

  if is_grade_mode then
    Format.printf "Grading Mode: Expression is typechecked.@\n"
  else 
    let _ = 
      Format.printf "Evaluating the expression...@\n";
      Format.print_flush ()
    in 
    let v = Eval.eval VarMap.empty e in 
    let _ = 
      Format.printf "Result:@\n  @[";
      Pprint.printVal v;
      Format.printf "@]@\n";
      Format.printf "@]"
    in 
    ()
