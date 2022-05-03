open Ast
open Eval
open Rename

type backend =
    EvalBackend
  | BytecodeBackend

type driver_options = {
  filename : string;
  print_ast : bool;
  print_renamed : bool;
  backend : backend
}

exception ParseError of loc


let parse_and_rename (options : driver_options) (lexbuf : Lexing.lexbuf) (scope : RenameScope.t) : NameExpr.expr list * RenameScope.t =
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
  renamed, new_scope


let run_env (options : driver_options) (lexbuf : Lexing.lexbuf) (env : eval_env) (scope : RenameScope.t) : value * eval_env * RenameScope.t = 
  let _ = match options.backend with
  | EvalBackend -> ()
  | BytecodeBackend -> raise (Util.Panic "The bytecode backend does not support incremental evaluation")
  in

  let renamed, new_scope = parse_and_rename options lexbuf scope in
  
  let res, new_env = eval_seq_state env renamed in
  res, new_env, new_scope

let run_eval (options : driver_options) (lexbuf : Lexing.lexbuf) : value =
  let _ = match options.backend with
  | EvalBackend -> ()
  | BytecodeBackend -> raise (Util.Panic "The bytecode backend does not support value evaluation")
  in
  let res, _, _ = run_env options lexbuf empty_eval_env RenameScope.empty in
  res

let run (options : driver_options) (lexbuf : Lexing.lexbuf) : unit =
  match options.backend with
  | EvalBackend ->   
    let _ = run_eval options lexbuf in
    ()
  | BytecodeBackend ->
    let renamed, _ = parse_and_rename options lexbuf RenameScope.empty in
    let bytecode = Compile.compile renamed in
    print_endline (Bytecode.pretty bytecode)
  
    