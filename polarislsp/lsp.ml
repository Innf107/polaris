type position = {
  line : int;
  character : int;
}
[@@deriving yojson]

type range = {
  start : position;
  end_ : position; [@key "end"]
}
[@@deriving yojson]

type document_uri = string [@@deriving yojson]

type text_document_item = {
  uri : document_uri;
  languageId : string;
  version : int;
  text : string;
}
[@@deriving yojson]

type text_document_identifier = { uri : document_uri } [@@deriving yojson]

type progress_token =
  | IntToken of int
  | StringToken of string

let progress_token_of_yojson = function
  | `String str -> StringToken str
  | `Int intlit -> IntToken intlit
  | _ -> Yojson.json_error "Invalid progress token"

let yojson_of_progress_token = function
  | IntToken int -> `Int int
  | StringToken str -> `String str

type hover_params = {
  textDocument : text_document_identifier;
  position : position;
  workDoneToken : progress_token option; [@yojson.option]
}
[@@deriving yojson]

type versioned_text_document_identifier = {
  uri : document_uri;
  version : int;
}
[@@deriving yojson]

type text_document_content_change_event = {
  (* This is None if the client sent the whole document *)
  range : range option; [@yojson.option]
  text : string;
}
[@@deriving yojson]

type did_change_params = {
  textDocument : versioned_text_document_identifier;
  contentChanges : text_document_content_change_event array;
}
[@@deriving yojson]

type did_open_params = { textDocument : text_document_item } [@@deriving yojson]

type client_notification =
  | DidChange of did_change_params
  | DidOpen of did_open_params

type client_request =
  | Initialize (* TODO: Missing initialize_params *)
  | Hover of hover_params

let parse_client_request : string -> Yojson.Safe.t -> client_request option =
 fun req_method json ->
  match req_method with
  | "initialize" -> Some Initialize
  | "textDocument/hover" -> Some (Hover (hover_params_of_yojson json))
  | _ -> None

let parse_client_notification :
    string -> Yojson.Safe.t -> client_notification option =
 fun req_method json ->
  match req_method with
  | "textDocument/didChange" ->
      Some (DidChange (did_change_params_of_yojson json))
  | "textDocument/didOpen" -> Some (DidOpen (did_open_params_of_yojson json))
  | _ -> None
