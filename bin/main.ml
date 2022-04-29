open Script
open Script.Ast
open Script.Eval

let test = let open StringExpr in [
    LetSeq ("x", (Lambda (["x"], IntLit 5)))
  ; App (Var "x", [Var "x"])
  ]

let fatal_error (message : string) = 
  print_endline "~~~~~~~~~~~~~~ERROR~~~~~~~~~~~~~~";
  print_endline message;
  print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
  exit 1

let run (exprs : string_expr list) = 
  let open Rename.RenameError in 
  let open Eval.EvalError in
  try
    let renamed = Rename.rename exprs in
    print_endline "~~~~~~~~Renamed AST~~~~~~~";
    print_endline (NameExpr.pretty_list renamed);
    print_endline "~~~~~~~~Eval~~~~~~~~~~~~~~";
    print_endline (Value.pretty (eval (NameExpr.Seq renamed)))
  with
  (* RenameError *)
  | VarNotFound x -> fatal_error ("Variable not found: '" ^ x ^ "'")
  | LetSeqInNonSeq expr -> fatal_error (
        "Let expression without 'in' found outside a sequence expression.\n"
      ^ "    Expression: " ^ StringExpr.pretty expr
      )
  (* EvalError *)
  | DynamicVarNotFound x -> fatal_error (
        "Variavle not found during execution: '" ^ Name.pretty x ^ "'\n"
      ^ "This variable was either generated dynamically, or there is a bug in the interpreter"
      )

let () = 
  run test

