let run_polaris ~filename lexbuf =
  let driver_options =
    Polaris.Driver.
      {
        filename;
        argv = [];
        print_ast = false;
        print_renamed = false;
        print_typed = false;
        print_tokens = false;
      }
  in

  Polaris.Driver.parse_rename_typecheck driver_options lexbuf
    Polaris.Rename.RenameScope.empty Polaris.Types.empty_env

let try_update_model ~filename lexbuf =
  let error_to_diagnostics err =
    let text_style = Polaris.Errormessage.make_text_style ~enable_color:false in

    let diagnostics = ref [] in

    Polaris.Error.pretty_error text_style
      (fun mloc message ->
        let loc = Option.value ~default:Polaris.Loc.internal mloc in
        let diagnostic =
          Diagnostic.{ loc; severity = `Error; source = "polaris"; message }
        in
        diagnostics := diagnostic :: !diagnostics)
      err;

    !diagnostics
  in

  match run_polaris ~filename lexbuf with
  | Error err -> (error_to_diagnostics err, None)
  | Ok (typed_header, typed_exprs, _rename_scope, _global_type_env, type_errors)
    ->
      ( error_to_diagnostics (Polaris.Error.TypeErrors type_errors),
        Some (Model.build typed_exprs) )
