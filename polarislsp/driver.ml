
let run_polaris ~filename lexbuf = 
  let driver_options = Polaris.Driver.{
    filename;
    argv = [];
    print_ast = false;
    print_renamed = false;
    print_tokens = false;
  } in

  Polaris.Driver.parse_rename_typecheck 
    driver_options
    lexbuf 
    Polaris.Rename.RenameScope.empty


let update_diagnostics ~filename lexbuf =
  try
    let _typed_header, _typed_exprs, _rename_scope, _global_type_env = run_polaris ~filename lexbuf in
    []
  with
  | Polaris.Driver.ParseError (loc, msg) -> [Diagnostic.{
      loc;
      severity = `Error;
      source = "polaris";
      message = "Parse Error: " ^ msg
    }]

