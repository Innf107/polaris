let run_polaris ~scope_registration ~filename lexbuf =
  let driver_options =
    Polaris.Driver.
      {
        filename;
        argv = [];
        print_ast = false;
        print_renamed = false;
        print_tokens = false;
        scope_registration;
      }
  in

  Polaris.Driver.parse_rename_typecheck driver_options lexbuf
    Polaris.Rename.RenameScope.empty Polaris.Types.empty_env

let try_update_model ~filename lexbuf =
  let on_error err =
    let text_style = Polaris.Errormessage.make_text_style ~enable_color:false in

    let mloc, message =
      Polaris.Error.pretty_error text_style (fun loc msg -> (loc, msg)) err
    in
    let loc = Option.value ~default:Polaris.Loc.internal mloc in

    ( [ Diagnostic.{ loc; severity = `Error; source = "polaris"; message } ],
      None )
  in

  let scope_collector = Model.ScopeCollection.new_collector () in

  match
    run_polaris
      ~scope_registration:
        (Model.ScopeCollection.registration_for scope_collector)
      ~filename lexbuf
  with
  | Error err -> on_error err
  | Ok (typed_header, typed_exprs, _rename_scope, _global_type_env) ->
      let built_scopes = Model.ScopeCollection.collect scope_collector in
      ([], Some (Model.build built_scopes typed_exprs))
