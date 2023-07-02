open Syntax
open Rename
open Eval
module FilePathMap = Map.Make (String)

let _tc_category, trace_driver = Trace.make ~flag:"driver" ~prefix:"Driver"

type driver_options = {
  filename : string;
  argv : string list;
  print_ast : bool;
  print_renamed : bool;
  print_tokens : bool;
}

let rec parse_rename_typecheck :
    driver_options ->
    Lexing.lexbuf ->
    RenameScope.t ->
    ?check_or_infer_top_level:[ `Check | `Infer ] ->
    Types.global_env ->
    ( Typed.header * Typed.expr list * RenameScope.t * Types.global_env,
      Error.t )
    result =
 fun options lexbuf scope ?(check_or_infer_top_level = `Check) type_env ->
  Error.handle_errors
    (fun err -> Error err)
    begin
      fun () ->
        trace_driver (lazy ("Lexing with filename '" ^ options.filename));
        Lexing.set_filename lexbuf options.filename;

        if options.print_tokens then
          let lex_state = Lexer.new_lex_state () in
          let rec go () =
            match Lexer.token lex_state lexbuf with
            | Parser.EOF -> exit 0
            | t ->
                print_endline (Parserutil.pretty_token t);
                go ()
          in
          go ()
        else ();

        trace_driver (lazy "Parsing...");
        let header, ast =
          let lex_state = Lexer.new_lex_state () in
          match Parser.main (Lexer.token lex_state) lexbuf with
          | exception Parser.Error ->
              let start_pos = lexbuf.lex_start_p in
              let end_pos = lexbuf.lex_curr_p in
              raise
                (Parserprelude.ParseError
                   (Loc.from_pos start_pos end_pos, "Parse error"))
          | res -> res
        in
        if options.print_ast then begin
          print_endline "~~~~~~~~Parsed AST~~~~~~~~";
          print_endline (Parsed.pretty_list ast);
          print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~"
        end
        else ();

        let imported_files =
          List.map
            (fun x -> (x, Util.path_relative_to options.filename x))
            (snd
               (Parsed.Traversal.traverse_list
                  Modules.extract_import_paths#traverse_expr [] ast))
        in

        trace_driver
          (lazy
            ("Importing modules from ("
            ^ String.concat ", " (List.map snd imported_files)
            ^ ")"));

        let items_for_exports =
          List.map
            (fun (filename, path) ->
              ( filename,
                Error.as_exn
                  (parse_rename_typecheck
                     { options with filename = path }
                     (Lexing.from_channel (open_in path))
                     RenameScope.empty Types.empty_env) ))
            imported_files
        in

        let import_map =
          FilePathMap.of_seq
            (Seq.map
               (fun (file, (header, ast, scope, env)) ->
                 (file, (Modules.build_export_map header ast scope env, ast)))
               (List.to_seq items_for_exports))
        in

        trace_driver (lazy "Renaming...");
        let renamed_header, renamed, new_scope =
          Rename.rename_scope import_map scope header ast
        in
        if options.print_renamed then begin
          print_endline "~~~~~~~~Renamed AST~~~~~~~";
          print_endline (Renamed.pretty_list renamed);
          print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~"
        end
        else ();

        trace_driver (lazy "Typechecking...");
        let type_env, typed_header, typed_exprs =
          Types.typecheck check_or_infer_top_level renamed_header renamed
            type_env
        in

        Ok (typed_header, typed_exprs, new_scope, type_env)
    end

let run_env :
    driver_options ->
    Lexing.lexbuf ->
    eval_env ->
    RenameScope.t ->
    ?check_or_infer_top_level:[ `Check | `Infer ] ->
    Types.global_env ->
    fs:Eio.Fs.dir Eio.Path.t ->
    mgr:Eio.Process.mgr ->
    (value * eval_env * RenameScope.t * Types.global_env, Error.t) result =
 fun options lexbuf env scope ?check_or_infer_top_level type_env ~fs ~mgr ->
  Error.handle_errors
    (fun err -> Error err)
    begin
      fun () ->
        let ( let* ) = Result.bind in

        let* renamed_header, renamed, new_scope, new_type_env =
          parse_rename_typecheck options lexbuf scope ?check_or_infer_top_level
            type_env
        in

        trace_driver (lazy "Evaluating...");
        let env = Eval.eval_header env renamed_header in
        let context =
          match check_or_infer_top_level with
          | Some `Check
          | None ->
              `Statement
          | Some `Infer -> `Expr
        in
        Eio.Switch.run
          begin
            fun switch ->
              let res, new_env =
                Eval.eval_seq_state ~cap:{ switch; fs; mgr } context env renamed
              in
              Ok (res, new_env, new_scope, new_type_env)
          end
    end

let run (options : driver_options) (lexbuf : Lexing.lexbuf) ~fs ~mgr :
    (value, Error.t) result =
  let result =
    run_env ~fs ~mgr options lexbuf
      (Eval.make_eval_env options.argv)
      RenameScope.empty Types.empty_env
  in
  Result.map (fun (value, _, _, _) -> value) result
