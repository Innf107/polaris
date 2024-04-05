module Json = Protocol_conv_json.Json

type ('a, 'b) untagged_either = ('a, 'b) Either.t

let untagged_either_to_json left right = function
  | Either.Left x -> left x
  | Either.Right x -> right x

let untagged_either_of_json_exn left right yojson =
  match left yojson with
  | value -> Either.Left value
  | exception Yojson.Json_error left_message -> (
      match right yojson with
      | value -> Either.Right value
      | exception Yojson.Json_error right_message ->
          Yojson.json_error
            ("Parsed value does not match left alternative: '" ^ left_message
           ^ "' or right alternative: '" ^ right_message ^ "'"))

type lsp_any = Yojson.Safe.t

let lsp_any_of_json_exn = Fun.id
let lsp_any_to_json = Fun.id

type position = {
  line : int;
  character : int;
}
[@@deriving protocol ~driver:(module Json)]

type range = {
  start : position;
  end_ : position; [@key "end"]
}
[@@deriving protocol ~driver:(module Json)]

type document_uri = string [@@deriving protocol ~driver:(module Json)]

type location = {
  uri : document_uri;
  range : range;
}
[@@deriving protocol ~driver:(module Json)]

type text_document_item = {
  uri : document_uri;
  languageId : string;
  version : int;
  text : string;
}
[@@deriving protocol ~driver:(module Json)]

type text_document_identifier = { uri : document_uri }
[@@deriving protocol ~driver:(module Json)]

type progress_token =
  | IntToken of int
  | StringToken of string

let progress_token_of_json_exn = function
  | `String str -> StringToken str
  | `Int intlit -> IntToken intlit
  | _ -> Yojson.json_error "Invalid progress token"

let progress_token_to_json = function
  | IntToken int -> `Int int
  | StringToken str -> `String str

type hover_params = {
  textDocument : text_document_identifier;
  position : position;
  workDoneToken : progress_token option; [@default None]
}
[@@deriving protocol ~driver:(module Json)]

type definition_params = {
  textDocument : text_document_identifier;
  position : position;
  workDoneToken : progress_token option; [@default None]
  partialResultToken : progress_token option; [@default None]
}
[@@deriving protocol ~driver:(module Json)]

type completion_trigger_kind =
  | Invoked
  | TriggerCharacter
  | TriggerForIncompleteCompletions

let completion_trigger_kind_to_json = function
  | Invoked -> `Int 1
  | TriggerCharacter -> `Int 2
  | TriggerForIncompleteCompletions -> `Int 3

let completion_trigger_kind_of_json_exn = function
  | `Int 1 -> Invoked
  | `Int 2 -> TriggerCharacter
  | `Int 3 -> TriggerForIncompleteCompletions
  | value ->
      Yojson.json_error
        ("Invalid CompletionTriggerKind: " ^ Yojson.Safe.show value)

type completion_context = {
  triggerKind : completion_trigger_kind;
  triggerCharacter : string option; [@default None]
}
[@@deriving protocol ~driver:(module Json)]

type completion_params = {
  textDocument : text_document_identifier;
  position : position;
  workDoneToken : progress_token option; [@default None]
  partialResultToken : progress_token option; [@default None]
  context : completion_context;
}
[@@deriving protocol ~driver:(module Json)]

type insert_text_format =
  | PlainText
  | Snippet

let insert_text_format_to_json = function
  | PlainText -> `Int 1
  | Snippet -> `Int 2

let insert_text_format_of_json_exn = function
  | `Int 1 -> PlainText
  | `Int 2 -> Snippet
  | yojson ->
      Yojson.json_error
        ("invalid insert_text_format: " ^ Yojson.Safe.show yojson)

type insert_text_mode =
  | AsIs
  | AdjustIndentation

let insert_text_mode_of_json_exn = function
  | `Int 1 -> AsIs
  | `Int 2 -> AdjustIndentation
  | yojson ->
      Yojson.json_error ("invalid insert_text_mode: " ^ Yojson.Safe.show yojson)

let insert_text_mode_to_json = function
  | AsIs -> `Int 1
  | AdjustIndentation -> `Int 2

type item_defaults = {
  commit_characters : string list option; [@default None]
  (* TODO: this is technically also allowed to be {insert: Range, replace: Range}. is that relevant for us? *)
  edditRange : range option; [@default None]
  insertTextFormat : insert_text_format option; [@default None]
  insertTextMode : insert_text_mode option; [@default None]
      (* TODO: there is also a `data?: LSPAny` field. i have no idea what this does, why we would need it,
         or how we would even represent it though, so we can hopefully leave it off for now.*)
}
[@@deriving protocol ~driver:(module Json)]

type completion_item_label_details = {
  detail : string option; [@default None]
  description : string option; [@default None]
}
[@@deriving protocol ~driver:(module Json)]

type completion_item_kind =
  | Text
  | Method
  | Function
  | Constructor
  | Field
  | Variable
  | Class
  | Interface
  | Module
  | Property
  | Unit
  | Value
  | Enum
  | Keyword
  | Snippet
  | Color
  | File
  | Reference
  | Folder
  | EnumMember
  | Constant
  | Struct
  | Event
  | Operator
  | TypeParameter

let completion_item_kind_to_json = function
  | Text -> `Int 1
  | Method -> `Int 2
  | Function -> `Int 3
  | Constructor -> `Int 4
  | Field -> `Int 5
  | Variable -> `Int 6
  | Class -> `Int 7
  | Interface -> `Int 8
  | Module -> `Int 9
  | Property -> `Int 10
  | Unit -> `Int 11
  | Value -> `Int 12
  | Enum -> `Int 13
  | Keyword -> `Int 14
  | Snippet -> `Int 15
  | Color -> `Int 16
  | File -> `Int 17
  | Reference -> `Int 18
  | Folder -> `Int 19
  | EnumMember -> `Int 20
  | Constant -> `Int 21
  | Struct -> `Int 22
  | Event -> `Int 23
  | Operator -> `Int 24
  | TypeParameter -> `Int 25

let completion_item_kind_of_json_exn = function
  | `Int 1 -> Text
  | `Int 2 -> Method
  | `Int 3 -> Function
  | `Int 4 -> Constructor
  | `Int 5 -> Field
  | `Int 6 -> Variable
  | `Int 7 -> Class
  | `Int 8 -> Interface
  | `Int 9 -> Module
  | `Int 10 -> Property
  | `Int 11 -> Unit
  | `Int 12 -> Value
  | `Int 13 -> Enum
  | `Int 14 -> Keyword
  | `Int 15 -> Snippet
  | `Int 16 -> Color
  | `Int 17 -> File
  | `Int 18 -> Reference
  | `Int 19 -> Folder
  | `Int 20 -> EnumMember
  | `Int 21 -> Constant
  | `Int 22 -> Struct
  | `Int 23 -> Event
  | `Int 24 -> Operator
  | `Int 25 -> TypeParameter
  | yojson ->
      Yojson.json_error
        ("invalid completion_item_kind: " ^ Yojson.Safe.show yojson)

type completion_item_tag = Deprecated

let completion_item_tag_to_json Deprecated = `Int 1

let completion_item_tag_of_json_exn = function
  | `Int 1 -> Deprecated
  | yojson ->
      Yojson.json_error
        ("invalid completion_item_tag: " ^ Yojson.Safe.show yojson)

type markup_kind =
  | PlainText
  | Markdown

let markup_kind_to_json = function
  | PlainText -> `String "plaintext"
  | Markdown -> `String "markdown"

let markup_kind_of_json_exn = function
  | `String "plaintext" -> PlainText
  | `String "markdown" -> Markdown
  | yojson ->
      Yojson.json_error ("invalid markup_kind: " ^ Yojson.Safe.show yojson)

type markup_content = {
  kind : markup_kind;
  value : string;
}
[@@deriving protocol ~driver:(module Json)]

type text_edit = {
  range : range;
  newText : string;
}
[@@deriving protocol ~driver:(module Json)]

type insert_replace_edit = {
  newText : string;
  insert : range;
  replace : range;
}
[@@deriving protocol ~driver:(module Json)]

type command = {
  title : string;
  command : string;
  arguments : lsp_any list option; [@default None]
}
[@@deriving protocol ~driver:(module Json)]

(* TODO: this is so large that it might make more sense to represent it differently in memory
   if we are going to construct a few hundred of these (e.g. as an already computed yojson object?)*)
type completion_item = {
  label : string;
  labelDetails : completion_item_label_details option; [@default None]
  kind : completion_item_kind option; [@default None]
  tags : completion_item_tag list option; [@default None]
  detail : string option; [@default None]
  documentation : (string, markup_content) untagged_either option;
      [@default None]
  deprecated : bool option; [@default None]
  preselect : bool option; [@default None]
  sortText : string option; [@default None]
  filterText : string option; [@default None]
  insertText : string option; [@default None]
  insertTextFormat : insert_text_format option; [@default None]
  insertTextMode : insert_text_mode option; [@default None]
  textEdit : (text_edit, insert_replace_edit) untagged_either option;
      [@default None]
  textEditText : string option; [@default None]
  additionalTextEdits : text_edit list option; [@default None]
  commitCharacters : string list option; [@default None]
  command : command option; [@default None]
      (* TODO: there is a data?: LSPAny field that i don't know what to do with *)
}
[@@deriving protocol ~driver:(module Json)]

(* fuck you ppx *)
let make_completion_item ?labelDetails ?kind ?tags ?detail ?documentation
    ?deprecated ?preselect ?sortText ?filterText ?insertText ?insertTextFormat
    ?insertTextMode ?textEdit ?textEditText ?additionalTextEdits
    ?commitCharacters ?command label =
  {
    label;
    labelDetails;
    kind;
    tags;
    detail;
    documentation;
    deprecated;
    preselect;
    sortText;
    filterText;
    insertText;
    insertTextFormat;
    insertTextMode;
    textEdit;
    textEditText;
    additionalTextEdits;
    commitCharacters;
    command;
  }

type completion_list = {
  isIncomplete : bool;
  itemDefaults : item_defaults option; [@default None]
  items : completion_item list;
}
[@@deriving protocol ~driver:(module Json)]

type versioned_text_document_identifier = {
  uri : document_uri;
  version : int;
}
[@@deriving protocol ~driver:(module Json)]

type text_document_content_change_event = {
  (* This is None if the client sent the whole document *)
  range : range option; [@default None]
  text : string;
}
[@@deriving protocol ~driver:(module Json)]

type did_change_params = {
  textDocument : versioned_text_document_identifier;
  contentChanges : text_document_content_change_event array;
}
[@@deriving protocol ~driver:(module Json)]

type did_open_params = { textDocument : text_document_item }
[@@deriving protocol ~driver:(module Json)]

type client_notification =
  | DidChange of did_change_params
  | DidOpen of did_open_params

type client_request =
  | Initialize (* TODO: Missing initialize_params *)
  | Hover of hover_params
  | Definition of definition_params
  | Completion of completion_params

let parse_client_request : string -> Yojson.Safe.t -> client_request option =
 fun req_method json ->
  match req_method with
  | "initialize" -> Some Initialize
  | "textDocument/hover" -> Some (Hover (hover_params_of_json_exn json))
  | "textDocument/definition" ->
      Some (Definition (definition_params_of_json_exn json))
  | "textDocument/completion" ->
      Some (Completion (completion_params_of_json_exn json))
  | _ -> None

let parse_client_notification :
    string -> Yojson.Safe.t -> client_notification option =
 fun req_method json ->
  match req_method with
  | "textDocument/didChange" ->
      Some (DidChange (did_change_params_of_json_exn json))
  | "textDocument/didOpen" -> Some (DidOpen (did_open_params_of_json_exn json))
  | _ -> None
