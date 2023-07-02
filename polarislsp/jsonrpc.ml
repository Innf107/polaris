let _request_category, trace_request =
  Trace.make ~flag:"request" ~prefix:"REQUEST"

let _response_category, trace_response =
  Trace.make ~flag:"response" ~prefix:"RESPONSE"

exception UnsupportedMethod
exception ParseError of string

let unsupported_method () = raise UnsupportedMethod
let parse_error message = raise (ParseError message)

let input_line_cr in_channel =
  match In_channel.input_line in_channel with
  | None -> None
  | Some "" -> Some ""
  | Some str ->
      (* Strip the final \r *)
      Some (String.sub str 0 (String.length str - 1))

let output_endline_cr out_channel msg =
  Out_channel.output_string out_channel (msg ^ "\r\n")

let output_newline_cr out_channel = Out_channel.output_string out_channel "\r\n"

let parse_request_to_string in_channel =
  let rec parse known_length =
    match input_line_cr in_channel with
    | None -> raise End_of_file
    | Some "" -> begin
        match known_length with
        | None ->
            raise
              (Failure "Error parsing request: Missing Content-Length header")
        | Some length ->
            let buffer = Bytes.make length '\n' in

            begin
              match In_channel.really_input in_channel buffer 0 length with
              | None -> raise End_of_file
              | Some () -> String.of_bytes buffer
            end
      end
    | Some line -> (
        match Base.String.chop_prefix line ~prefix:"Content-Length:" with
        | None -> parse known_length
        | Some content_length_string -> (
            match int_of_string_opt (String.trim content_length_string) with
            | None ->
                raise
                  (Failure
                     ("Error parsing request: Invalid content length: '"
                    ^ content_length_string ^ "'"))
            | Some length -> parse (Some length)))
  in
  parse None

let rpc_error ?(data = `Null) request_id code message =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", request_id);
      ( "error",
        `Assoc
          [ ("code", `Int code); ("message", `String message); ("data", data) ]
      );
    ]

let send_response out_channel response =
  let response_string = Yojson.Safe.to_string response in
  trace_response (lazy response_string);
  let response_size = String.length response_string in
  output_endline_cr out_channel
    ("Content-Length: " ^ string_of_int response_size);
  output_newline_cr out_channel;
  output_string out_channel response_string;
  flush out_channel

let run in_channel out_channel ~handler ~notification_handler =
  try
    while true do
      try
        let body_string = parse_request_to_string in_channel in
        trace_request (lazy body_string);

        let request = Yojson.Safe.from_string body_string in

        let request_method = Jsonutil.field "method" Jsonutil.string request in
        let request_params = Jsonutil.field_opt "params" Jsonutil.any request in
        let request_id = Jsonutil.field_opt "id" Jsonutil.any request in

        (* We pass null as the parameter in case it doesn't exist on the request.
           This is technically not spec compliant but should be fine for us *)
        let request_params = Option.value ~default:`Null request_params in

        match request_id with
        | None -> begin
            try
              notification_handler ~notification_method:request_method
                request_params
            with
            | ParseError message ->
                send_response out_channel
                  (rpc_error `Null (-32700) ("Parse error: " ^ message))
            (* This is a notification, so we don't send a response *)
          end
        | Some request_id ->
            let response =
              try
                let response_data = handler ~request_method request_params in
                `Assoc
                  [
                    ("jsonrpc", `String "2.0");
                    ("result", response_data);
                    ("id", request_id);
                  ]
              with
              | UnsupportedMethod ->
                  rpc_error
                    ?data:(Some (`String request_method))
                    request_id (-32601)
                    ("Unsupported method: '" ^ request_method ^ "'")
              | ParseError message ->
                  rpc_error request_id (-32700) ("Parse error: " ^ message)
            in

            send_response out_channel response
      with
      | Failure message -> prerr_endline ("Error: " ^ message)
      (* Bubble up to exit the outer loop *)
      | End_of_file -> raise End_of_file
      | err -> prerr_endline ("Error: " ^ Printexc.to_string err)
    done
  with
  | End_of_file -> prerr_endline "Exiting on EOF"

(* TODO: Send this to an implicit(?) out channel *)
let send_notification notification_method data =
  let out_channel = stdout in

  let json =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("method", `String notification_method);
        ("params", data);
      ]
  in
  let json_string = Yojson.Safe.to_string json in
  trace_response (lazy ("[NOTIFICATION]: " ^ json_string));
  let size = String.length json_string in
  output_endline_cr out_channel ("Content-Length: " ^ string_of_int size);
  output_newline_cr out_channel;
  output_string out_channel json_string;
  flush out_channel

let position =
  Jsonutil.make_parser "position" (fun json ->
      let ( let* ) = Option.bind in
      let* line = Jsonutil.field_opt "line" Jsonutil.int json in
      let* character = Jsonutil.field_opt "character" Jsonutil.int json in
      Some (line, character))
