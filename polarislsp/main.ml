
let usage_message = 
{|Usage: polarislsp [--trace CATEGORY]

    Options:
      --trace <category>   Enable traces for a given category.
                           Possible values: |} ^ String.concat ", " (Trace.get_categories ())


let fail_usage message =
  prerr_endline message;
  prerr_newline ();
  prerr_endline usage_message;
  exit 1

let rec parse_args = function
  | [] | [""] -> ()
  | ("--help" :: _) -> 
    print_endline usage_message;
    exit 0
  | ["--trace"] -> fail_usage "Missing required argument for flag '--trace'"
  | ("--trace" :: category :: rest) ->
    let is_valid_category = Trace.try_set_enabled category true in
    if not is_valid_category then begin
      fail_usage ("Invalid trace category '" ^ category ^ "'. Possible values: " ^ String.concat ", " (Trace.get_categories ()))
    end;
    parse_args rest
  | (flag :: _) -> fail_usage ("Invalid option: '" ^ flag ^ "'")


let on_initialize body = 
  Yojson.Basic.from_string {|{ 
    "capabilities": {
      "hoverProvider": true,
      "textDocumentSync": {
        "change": 1
      }
    }, 
    "serverInfo": {
        "name": "polarislsp"
      }
    }
  |}

let filename_of_uri file =
  Base.String.chop_prefix_exn file ~prefix:"file://" 

let on_change model_ref body =
  let open Jsonutil in
  let file_uri = nested_field ["textDocument"; "uri"] string body in
  let content_changes = Jsonutil.field "contentChanges" (list (assoc string)) body in
  let content = match content_changes with
  | [[("text", content)]] -> content
  | _ -> raise (Failure "Unexpected content changes in textDocument/didChange notification")
  in

  let filename = filename_of_uri file_uri in

  let diagnostics, model_option = Driver.try_update_model ~filename (Lexing.from_string content) in

  begin match model_option with
  | None -> ()
  | Some new_model -> 
    model_ref := Some new_model
  end;

  Jsonrpc.send_notification 
    "textDocument/publishDiagnostics" 
    (`Assoc [
      "uri", `String file_uri;
      "diagnostics", `List (List.map Diagnostic.to_json diagnostics)
    ])

let on_hover model_ref body =
  let model = !model_ref in

  let no_response () = `Null in

  match model with
  | None -> no_response ()
  | Some model ->
    let file_uri = Jsonutil.nested_field ["textDocument"; "uri"] Jsonutil.string body in
    let position = Jsonutil.field "position" Jsonrpc.position body in
    
    let file = filename_of_uri file_uri in
    
    match Model.find_hover_entry_at ~file position model with
    | Some (loc, (Model.Var (var_name, ty) | Model.VarPattern (var_name, ty))) ->
      `Assoc [
        "range", Diagnostic.loc_to_json loc;
        "contents", `Assoc [
          "language", `String "polaris";
          "value", `String (
            Polaris.Syntax.Name.pretty var_name ^ " : " ^ Polaris.Syntax.Typed.pretty_type ty)
        ]
      ]
    | None -> no_response ()

let () =
  parse_args (List.tl (Array.to_list Sys.argv));

  let model_ref = ref None in

  prerr_endline "Running Polaris LSP server";
  Jsonrpc.run stdin stdout
    ~handler:(fun ~request_method -> 
      match request_method with
      | "initialize" -> on_initialize
      | "textDocument/hover" -> on_hover model_ref
      | _ -> fun _ -> Jsonrpc.unsupported_method ()
    )
    ~notification_handler:(fun ~notification_method -> 
      match notification_method with
      | "textDocument/didChange" -> on_change model_ref
      | _ -> fun _ -> Jsonrpc.unsupported_method ()  
    )

