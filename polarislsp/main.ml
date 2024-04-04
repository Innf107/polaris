let usage_message =
  {|Usage: polarislsp [--trace CATEGORY]

    Options:
      --trace <category>   Enable traces for a given category.
                           Possible values: |}
  ^ String.concat ", " (Trace.get_categories ())

let fail_usage message =
  prerr_endline message;
  prerr_newline ();
  prerr_endline usage_message;
  exit 1

let rec parse_args = function
  | []
  | [ "" ] ->
      ()
  | "--help" :: _ ->
      print_endline usage_message;
      exit 0
  | [ "--trace" ] -> fail_usage "Missing required argument for flag '--trace'"
  | "--trace" :: category :: rest ->
      let is_valid_category = Trace.try_set_enabled category true in
      if not is_valid_category then begin
        fail_usage
          ("Invalid trace category '" ^ category ^ "'. Possible values: "
          ^ String.concat ", " (Trace.get_categories ()))
      end;
      parse_args rest
  | flag :: _ -> fail_usage ("Invalid option: '" ^ flag ^ "'")

let on_initialize =
  Yojson.Safe.from_string
    {|{ 
    "capabilities": {
      "hoverProvider": true,
      "definitionProvider": true,
      "textDocumentSync": {
        "openClose": true,
        "change": 1
      },
      "completionProvider": {
        "triggerCharacters": [],
        "completionItem": {
          "labelDetailSupport": true
        }
      }
    }, 
    "serverInfo": {
        "name": "polarislsp"
      }
    }
  |}

let filename_of_uri file = Base.String.chop_prefix_exn file ~prefix:"file://"

let range_of_loc loc =
  let open Polaris.Loc in
  Lsp.
    {
      start = { line = loc.start_line - 1; character = loc.start_col - 1 };
      end_ = { line = loc.end_line - 1; character = loc.end_col - 1 };
    }

let process_document model_ref file_uri content =
  let filename = filename_of_uri file_uri in

  let diagnostics, model_option =
    Driver.try_update_model ~filename (Sedlexing.Utf8.from_string content)
  in

  begin
    match model_option with
    | None -> ()
    | Some new_model -> model_ref := Some new_model
  end;

  Jsonrpc.send_notification "textDocument/publishDiagnostics"
    (`Assoc
      [
        ("uri", `String file_uri);
        ("diagnostics", `List (List.map Diagnostic.to_json diagnostics));
      ])

let on_open model_ref (params : Lsp.did_open_params) =
  if params.textDocument.languageId = "polaris" then
    process_document model_ref params.textDocument.uri params.textDocument.text

let on_change model_ref (params : Lsp.did_change_params) =
  let content =
    match params.contentChanges with
    | [| { text; _ } |] -> text
    | _ ->
        raise
          (Failure
             "Unexpected content changes in textDocument/didChange notification")
  in
  process_document model_ref params.textDocument.uri content

let on_hover model_ref (hover_params : Lsp.hover_params) =
  let model = !model_ref in

  let no_response () = `Null in

  match model with
  | None -> no_response ()
  | Some model -> (
      let file_uri = hover_params.textDocument.uri in
      let file = filename_of_uri file_uri in

      let build_diagnostic loc message =
        `Assoc
          [
            ("range", Diagnostic.loc_to_json loc);
            ( "contents",
              `Assoc
                [ ("language", `String "polaris"); ("value", `String message) ]
            );
          ]
      in

      match Model.find_hover_entry_at ~file hover_params.position model with
      | Some (loc, Model.VarLike (var_name, ty)) ->
          build_diagnostic loc
            (Polaris.Syntax.Name.pretty var_name
            ^ " : "
            ^ Polaris.Syntax.Typed.pretty_type ty)
      | Some (loc, Model.Subscript (key, ty)) ->
          build_diagnostic loc
            ("." ^ key ^ " : " ^ Polaris.Syntax.Typed.pretty_type ty)
      | Some (loc, Model.Variant (name, ty)) ->
          build_diagnostic loc
            ("`" ^ name ^ " : " ^ Polaris.Syntax.Typed.pretty_type ty)
      | None -> no_response ())

let on_definition model_ref (params : Lsp.definition_params) =
  let no_response () = `Null in
  match !model_ref with
  | None -> no_response ()
  | Some model ->
      let file_uri = params.textDocument.uri in
      let file = filename_of_uri file_uri in
      begin
        match Model.find_definition_at ~file params.position model with
        | Some (_, definition_loc) ->
            let open Polaris.Loc in
            Lsp.location_to_json
              { uri = file_uri; range = range_of_loc definition_loc }
        | None -> no_response ()
      end

let on_completion model_ref (params : Lsp.completion_params) =
  `Null

let () =
  Eio_posix.run
    begin
      fun _ ->
        parse_args (List.tl (Array.to_list Sys.argv));

        let model_ref = ref None in

        prerr_endline "Running Polaris LSP server";
        Jsonrpc.run stdin stdout
          ~handler:(fun ~request_method body ->
            match Lsp.parse_client_request request_method body with
            | Some Initialize -> on_initialize
            | Some (Hover params) -> on_hover model_ref params
            | Some (Definition params) -> on_definition model_ref params
            | Some (Completion params) -> on_completion model_ref params
            | None -> Jsonrpc.unsupported_method ()
            | exception exn ->
                (* TODO: this is technically not a *parse* error*)
                Jsonrpc.parse_error ("Exception: " ^ Printexc.to_string exn))
          ~notification_handler:(fun ~notification_method body ->
            match Lsp.parse_client_notification notification_method body with
            | Some (DidOpen params) -> on_open model_ref params
            | Some (DidChange params) -> on_change model_ref params
            | None -> Jsonrpc.unsupported_method ()
            | exception exn ->
                Jsonrpc.parse_error ("Exception: " ^ Printexc.to_string exn))
    end
