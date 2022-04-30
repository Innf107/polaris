open Polaris
open Polaris.Ast
open Polaris.Eval

let fatal_error (message : string) = 
  print_endline "~~~~~~~~~~~~~~ERROR~~~~~~~~~~~~~~";
  print_endline message;
  print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
  exit 1

type run_options = {
  print_ast : bool
}

let run (options : run_options) (filepath : string): value = 
  let open Rename.RenameError in 
  let open Eval.EvalError in
  try
    let lexbuf = Lexing.from_channel (open_in filepath) in
    let ast = Parser.main Lexer.token lexbuf in

    let renamed = Rename.rename ast in
    if options.print_ast then begin
      print_endline "~~~~~~~~Renamed AST~~~~~~~";
      print_endline (NameExpr.pretty_list renamed);
      print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~"
    end
    else ();
    eval (NameExpr.Seq renamed)
  with
  | Parsing.Parse_error -> fatal_error "Parse error"
  | Sys_error msg -> fatal_error ("System error: " ^ msg)
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

let usage_message = "polaris [options] <FILE>"


let () =
  let args = ref [] in
  let anon_fun x = args := x :: !args in

  let print_ast = ref false in

  let speclist = [
    ("--print-renamed", Arg.Set print_ast, "Print the renamed syntax tree before evaluation")
  ] in
  Arg.parse speclist anon_fun usage_message;
  
  let options = {
      print_ast = !print_ast
    } in
  match !args with
    | [filepath] -> ignore (run options filepath)
    | _ -> Arg.usage speclist usage_message; exit 1