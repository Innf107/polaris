open Polaris
open Polaris.Ast
open Polaris.Eval

let fatal_error (message : string) = 
  print_endline "~~~~~~~~~~~~~~ERROR~~~~~~~~~~~~~~";
  print_endline message;
  print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
  exit 1

type run_options = {
  print_ast : bool;
  print_renamed : bool
}

let run (options : run_options) (filepath : string): value = 
  let open Rename.RenameError in 
  let open Eval.EvalError in
  try
    let lexbuf = Lexing.from_channel (open_in filepath) in 
    Lexing.set_filename lexbuf filepath;
    let ast = 
      try 
        Parser.main Lexer.token lexbuf 
      with 
      | Parser.Error -> 
        let start_pos = lexbuf.lex_start_p in
        let end_pos = lexbuf.lex_curr_p in 
        fatal_error (Printf.sprintf "Parse Error at %s:%d:%d-%d:%d" 
                        filepath 
                        start_pos.pos_lnum 
                        (start_pos.pos_cnum - start_pos.pos_bol + 1)
                        end_pos.pos_lnum 
                        (end_pos.pos_cnum - end_pos.pos_bol + 1))
    in
    if options.print_ast then begin
      print_endline "~~~~~~~~Parsed AST~~~~~~~~";
      print_endline (StringExpr.pretty_list ast);
      print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~"
    end
    else ();

    let renamed = Rename.rename ast in
    if options.print_renamed then begin
      print_endline "~~~~~~~~Renamed AST~~~~~~~";
      print_endline (NameExpr.pretty_list renamed);
      print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~"
    end
    else ();
    eval renamed
  with
  | Sys_error msg -> fatal_error ("System error: " ^ msg)
  (* RenameError *)
  | VarNotFound (x, loc) -> fatal_error (Loc.pretty loc ^ ": Variable not found: '" ^ x ^ "'")
  | LetSeqInNonSeq (expr, loc) -> fatal_error (
        Loc.pretty loc ^ ": Let expression without 'in' found outside a sequence expression.\n"
      ^ "    Expression: " ^ StringExpr.pretty expr
      )
  (* EvalError *)
  | DynamicVarNotFound (x, loc) -> fatal_error (
        Loc.pretty loc ^ ": Variable not found during execution: '" ^ Name.pretty x ^ "'\n"
      ^ "This is definitely a bug in the interpreter"
      )

let usage_message = "usage: polaris [options] <FILE>"


let () =
  let args = ref [] in
  let anon_fun x = args := x :: !args in

  let print_ast = ref false in
  let print_renamed = ref false in


  let speclist = [
    ("--print-ast", Arg.Set print_ast, "Print the parsed syntax tree before renaming");
    ("--print-renamed", Arg.Set print_renamed, "Print the renamed syntax tree before evaluation")
  ] in
  Arg.parse speclist anon_fun usage_message;
  
  let options = {
      print_ast = !print_ast;
      print_renamed = !print_renamed
    } in
  match !args with
    | [filepath] -> ignore (run options filepath)
    | _ -> Arg.usage speclist usage_message; exit 1