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


let polaris_home = Sys.getenv "HOME" ^ "/.polaris"

let rec parse_rename_typecheck : driver_options 
                              -> Lexing.lexbuf
                              -> RenameScope.t
                              -> Renamed.header * Renamed.expr list * RenameScope.t * Types.global_env
= fun options lexbuf scope ->
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

  (* TODO: I don't think options.argv[0] is the path of the script yet? *)
  let imported_files = List.map (fun x -> (x, Util.path_relative_to (List.hd options.argv) x)) 
                        (List.concat_map (Modules.extract_import_paths) ast) in

  let items_for_exports = List.map (fun (filename, path) -> 
      (filename, parse_rename_typecheck options (Lexing.from_channel (open_in path)) RenameScope.empty)
    ) imported_files in

  let import_map = FilePathMap.of_seq
      (Seq.map (fun (file, (header, ast, scope, env)) -> (file, (Modules.build_export_map header ast scope env, ast))) 
      (List.to_seq items_for_exports)) in

  let renamed_header, renamed, new_scope = Rename.rename_scope import_map scope header ast in 
  if options.print_renamed then begin
    print_endline "~~~~~~~~Renamed AST~~~~~~~";
    print_endline (Renamed.pretty_list renamed);
    print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~"
  end
  else ();

  let type_env = Types.typecheck renamed_header renamed in

  renamed_header, renamed, new_scope, type_env


let run_env (options : driver_options) (lexbuf : Lexing.lexbuf) (env : eval_env) (scope : RenameScope.t) : value * eval_env * RenameScope.t = 
  let renamed_header, renamed, new_scope, _new_ty_env = parse_rename_typecheck options lexbuf scope in
  
  let env = Eval.eval_header env renamed_header in
  let res, new_env = Eval.eval_seq_state env renamed in
  res, new_env, new_scope

let run_eval (options : driver_options) (lexbuf : Lexing.lexbuf) : value =
  let res, _, _ = run_env options lexbuf (Eval.empty_eval_env options.argv) RenameScope.empty in
  res

let run (options : driver_options) (lexbuf : Lexing.lexbuf) : unit =
  let _ = run_eval options lexbuf in 
  ()
