open Syntax
open Rename
open Eval
module FilePathMap = Trie.String

let _tc_category, trace_driver = Trace.make ~flag:"driver" ~prefix:"Driver"

type driver_options = {
  filename : string;
  argv : string list;
  print_ast : bool;
  print_renamed : bool;
  print_tokens : bool;
  scope_registration : Rename.scope_registration;
}

let ignored_scope_registration = Rename.ignored_scope_registration
let lex_parse_landmark = Landmark.register "lex_parse"
let rename_landmark = Landmark.register "rename"
let typecheck_landmark = Landmark.register "typecheck"

let std_files : string StringMap.t = StringMap.of_seq (List.to_seq Std_wrapper.by_filename)

let rec parse_rename_typecheck :
    driver_options ->
    Sedlexing.lexbuf ->
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
        Sedlexing.set_filename lexbuf options.filename;

        if options.print_tokens then
          let lex_state = Lexer.new_lex_state lexbuf in
          let rec go () =
            match Lexer.token lex_state with
            | Parser.EOF -> exit 0
            | t ->
                print_endline (Parserutil.pretty_token t);
                go ()
          in
          go ()
        else ();

        Landmark.enter lex_parse_landmark;
        trace_driver (lazy "Parsing...");
        let header, ast =
          let lex_state = Lexer.new_lex_state lexbuf in
          let lexer () =
            let token = Lexer.token lex_state in
            let start, end_ = Lexer.current_positions lex_state in
            (token, start, end_)
          in

          let open MenhirLib.Convert.Simplified in
          match traditional2revised Parser.main lexer with
          | exception Parser.Error ->
              let start_pos, end_pos = Sedlexing.lexing_positions lexbuf in
              raise
                (Parserprelude.ParseError
                   (Loc.from_pos start_pos end_pos, "Parse error"))
          | res -> res
        in
        Landmark.exit lex_parse_landmark;
        if options.print_ast then begin
          print_endline "~~~~~~~~Parsed AST~~~~~~~~";
          print_endline (Parsed.pretty_list ast);
          print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~"
        end
        else ();

        let _, imported_paths = Parsed.Traversal.traverse_list
          Modules.extract_import_paths#traverse_expr [] ast in

        let imported_files =
          List.map
            (fun (loc, path) -> (loc, path, Util.path_relative_to options.filename path))
            imported_paths
        in

        trace_driver
          (lazy
            ("Importing modules from ["
            ^ String.concat ", " (List.map (fun (_, _, full_path) -> full_path) imported_files)
            ^ "]"));

        let items_for_exports =
          List.map
            (fun (loc, filename, path) ->
              let lexbuf =
                match String.starts_with ~prefix:"@std/" filename with
                | true ->
                  let module_name = String.sub filename 5 (String.length filename - 5) in
                  begin match StringMap.find_opt module_name std_files with
                  | Some contents ->
                    let lexbuf = Sedlexing.Utf8.from_string contents in
                    Sedlexing.set_filename lexbuf filename;
                    lexbuf
                  | None -> raise (Error.ModuleError (ModuleStdlibFileNotFound (loc, module_name)))
                  end
                | false -> let in_channel = try open_in path with
                  (* Why can't i catch a file not found error here? *)
                  | Sys_error reason -> raise (Error.ModuleError (UnableToImportModule { loc; filename; reason }))
                  in
                  Sedlexing.Utf8.from_channel in_channel
              in
              let driver_options =
                {
                  options with
                  filename = path;
                  scope_registration = ignored_scope_registration;
                }
              in
              ( filename,
                Error.as_exn
                  (parse_rename_typecheck driver_options lexbuf
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

        Landmark.enter rename_landmark;
        trace_driver (lazy "Renaming...");
        let renamed_header, renamed, new_scope =
          Rename.rename_scope options.scope_registration import_map scope header
            ast
        in
        if options.print_renamed then begin
          print_endline "~~~~~~~~Renamed AST~~~~~~~";
          print_endline (Renamed.pretty_list renamed);
          print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~"
        end
        else ();
        Landmark.exit rename_landmark;

        Landmark.enter typecheck_landmark;
        trace_driver (lazy "Typechecking...");
        let type_env, typed_header, typed_exprs =
          Types.typecheck check_or_infer_top_level renamed_header renamed
            type_env
        in
        Landmark.exit typecheck_landmark;

        Ok (typed_header, typed_exprs, new_scope, type_env)
    end

let prt_landmark = Landmark.register "parse_rename_typecheck"
let eval_landmark = Landmark.register "eval"

let run_env :
    driver_options ->
    Sedlexing.lexbuf ->
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

        Landmark.enter prt_landmark;
        let* renamed_header, renamed, new_scope, new_type_env =
          parse_rename_typecheck options lexbuf scope ?check_or_infer_top_level
            type_env
        in
        Landmark.exit prt_landmark;

        Landmark.enter eval_landmark;
        trace_driver (lazy "Evaluating...");
        let env = Eval.eval_header env renamed_header in
        let context =
          match check_or_infer_top_level with
          | Some `Check
          | None ->
              `Statement
          | Some `Infer -> `Expr
        in
        let result =
          Eio.Switch.run
            begin
              fun switch ->
                let res, new_env =
                  Eval.eval_seq_state ~cap:{ switch; fs; mgr } context env
                    renamed
                in
                Ok (res, new_env, new_scope, new_type_env)
            end
        in
        Landmark.exit eval_landmark;
        result
    end

let run (options : driver_options) (lexbuf : Sedlexing.lexbuf) ~fs ~mgr :
    (value, Error.t) result =
  let result =
    run_env ~fs ~mgr options lexbuf
      (Eval.make_eval_env options.argv)
      RenameScope.empty Types.empty_env
  in
  Result.map (fun (value, _, _, _) -> value) result
