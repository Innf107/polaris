
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
  | [] -> ()
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

let on_change body =
  let file_uri = Jsonutil.nested_field ["textDocument"; "uri"] String body in
  let content_changes = Jsonutil.field "contentChanges" (List (Assoc String)) body in
  let content = match content_changes with
  | [[("text", content)]] -> content
  | _ -> raise (Failure "Unexpected content changes in textDocument/didChange notification")
  in

  let filename = filename_of_uri file_uri in

  let diagnostics = Driver.update_diagnostics ~filename (Lexing.from_string content) in

  Jsonrpc.send_notification 
    "textDocument/publishDiagnostics" 
    (`Assoc [
      "uri", `String file_uri;
      "diagnostics", `List (List.map Diagnostic.to_json diagnostics)
    ])

let on_hover body =
  Jsonrpc.unsupported_method ()

let () =
  parse_args (List.tl (Array.to_list Sys.argv));

  prerr_endline "Running Polaris LSP server";
  Jsonrpc.run stdin stdout
    ~handler:(fun ~request_method -> 
      match request_method with
      | "initialize" -> on_initialize
      | "textDocument/hover" -> on_hover
      | _ -> fun _ -> Jsonrpc.unsupported_method ()
    )
    ~notification_handler:(fun ~notification_method -> 
      match notification_method with
      | "textDocument/didChange" -> on_change
      | _ -> fun _ -> Jsonrpc.unsupported_method ()  
    )

