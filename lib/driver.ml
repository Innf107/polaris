open Ast
open Eval
open Rename

type driver_options = {
  filename : string;
  print_ast : bool;
  print_renamed : bool;
}

exception ParseError of loc


let run_env (options : driver_options) (lexbuf : Lexing.lexbuf) (env : eval_env) (scope : RenameScope.t) : value * eval_env * RenameScope.t = 
  Lexing.set_filename lexbuf options.filename;
  let ast = 
    try 
      Parser.main Lexer.token lexbuf 
    with 
    | Parser.Error -> 
      let start_pos = lexbuf.lex_start_p in
      let end_pos = lexbuf.lex_curr_p in 
      raise (ParseError (Loc.from_pos start_pos end_pos)) 
  in
  if options.print_ast then begin
    print_endline "~~~~~~~~Parsed AST~~~~~~~~";
    print_endline (StringExpr.pretty_list ast);
    print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~"
  end
  else ();

  let renamed, new_scope = Rename.rename_seq_state scope ast in
  if options.print_renamed then begin
    print_endline "~~~~~~~~Renamed AST~~~~~~~";
    print_endline (NameExpr.pretty_list renamed);
    print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~"
  end
  else ();
  let res, new_env = eval_seq_state env renamed in
  res, new_env, new_scope

let run (options : driver_options) (lexbuf : Lexing.lexbuf) : value =
  let res, _, _ = run_env options lexbuf empty_eval_env RenameScope.empty in
  res
  