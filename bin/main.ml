open Polaris
open Polaris.Ast
open Polaris.Eval

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
        "Variable not found during execution: '" ^ Name.pretty x ^ "'\n"
      ^ "This is definitely a bug in the interpreter"
      )

let () =
  try
    let lexbuf = Lexing.from_channel (open_in "test.pol") in
    let ast = Parser.main Lexer.token lexbuf in
    run ast
  with
    Parsing.Parse_error -> fatal_error "Parse error"
