open Syntax
open Rename
open Eval

type driver_options = {
  filename : string;
  argv : string list;
  print_ast : bool;
  print_renamed : bool;
  print_tokens : bool;
}

exception ParseError of loc * string


module type EvalI = sig
  val eval : string list -> Renamed.expr list -> value

  val eval_header : eval_env -> Renamed.header -> eval_env

  val eval_seq_state : eval_env -> Renamed.expr list -> value * eval_env

  val empty_eval_env : string list -> eval_env

end

module type DriverI = sig
  val run : driver_options -> Lexing.lexbuf -> unit

  val run_eval : driver_options -> Lexing.lexbuf -> value

  val run_env : driver_options -> Lexing.lexbuf -> eval_env -> RenameScope.t -> value * eval_env * RenameScope.t
end

let polaris_home = Sys.getenv "HOME" ^ "/.polaris"

module rec EvalInst : EvalI = Eval.Make(struct
  let eval_require scriptPathDir mod_path = 
    let driver_options = {
      filename = mod_path;
      argv = [mod_path];
      print_ast = false;
      print_renamed = false;
      print_tokens = false;
    } in
    let file_path = 
      if Filename.is_relative mod_path then  
        scriptPathDir ^ "/" ^ mod_path
      else
        mod_path
      in
    let stdlib_file_path = polaris_home ^ "/lib/" ^ mod_path in
    let in_chan = if Sys.file_exists file_path then
        In_channel.open_text file_path
      else if Sys.file_exists stdlib_file_path then
        In_channel.open_text stdlib_file_path
      else 
        raise (EvalError.ModuleNotFound (mod_path, [file_path; stdlib_file_path]))
    in
    Driver.run_eval driver_options (Lexing.from_channel in_chan)
end)
and Driver : DriverI = struct

  let parse_and_rename (options : driver_options) (lexbuf : Lexing.lexbuf) (scope : RenameScope.t) : Renamed.header * Renamed.expr list * RenameScope.t =
    Lexing.set_filename lexbuf options.filename;
    
    if options.print_tokens then
      let lex_state = Lexer.new_lex_state () in
      let rec go () =
        match Lexer.token lex_state lexbuf with
        | Parser.Token.EOF -> exit 0
        | t -> print_endline (Parser.Token.pretty t); go ()
      in
      go ()
    else
      ();

    let header, ast = 
      let lex_state = Lexer.new_lex_state () in
      let stream = Athena.Stream.of_iter begin fun () -> 
        match Lexer.token lex_state lexbuf with 
        | Parser.Token.EOF -> None 
        | x -> Some(x, Loc.from_pos lexbuf.lex_start_p lexbuf.lex_curr_p)
        end
      in
      match
        Parser.main stream 
      with 
      | Ok res -> res
      | Error(err) -> 
        let start_pos = lexbuf.lex_start_p in
        let end_pos = lexbuf.lex_curr_p in 
        raise (ParseError (Loc.from_pos start_pos end_pos, Parser.pretty_error err)) 
    in
    if options.print_ast then begin
      print_endline "~~~~~~~~Parsed AST~~~~~~~~";
      print_endline (Parsed.pretty_list ast);
      print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~"
    end
    else ();

    let renamed_header, new_scope = Rename.rename_header scope header in 
    let renamed, new_scope = Rename.rename_seq_state new_scope ast in
    if options.print_renamed then begin
      print_endline "~~~~~~~~Renamed AST~~~~~~~";
      print_endline (Renamed.pretty_list renamed);
      print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~"
    end
    else ();
    renamed_header, renamed, new_scope


  let run_env (options : driver_options) (lexbuf : Lexing.lexbuf) (env : eval_env) (scope : RenameScope.t) : value * eval_env * RenameScope.t = 
    let renamed_header, renamed, new_scope = parse_and_rename options lexbuf scope in
    
    Types.typecheck renamed;

    let env = EvalInst.eval_header env renamed_header in
    let res, new_env = EvalInst.eval_seq_state env renamed in
    res, new_env, new_scope

  let run_eval (options : driver_options) (lexbuf : Lexing.lexbuf) : value =
    let res, _, _ = run_env options lexbuf (EvalInst.empty_eval_env options.argv) RenameScope.empty in
    res

  let run (options : driver_options) (lexbuf : Lexing.lexbuf) : unit =
    let _ = run_eval options lexbuf in 
    ()
end