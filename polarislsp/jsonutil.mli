exception NonexistantField of string * Yojson.Safe.t
exception NonObjectDereference of string * Yojson.Safe.t
exception WrongTypeAsserted of string * Yojson.Safe.t

type 'a json_parser

val make_parser : string -> (Yojson.Safe.t -> 'a option) -> 'a json_parser
val any : Yojson.Safe.t json_parser
val bool : bool json_parser
val float : float json_parser
val int : int json_parser
val null : unit json_parser
val string : string json_parser
val list : 'a json_parser -> 'a list json_parser
val assoc : 'a json_parser -> (string * 'a) list json_parser
val coerce_json : 'a json_parser -> Yojson.Safe.t -> 'a option
val assert_json : 'a json_parser -> Yojson.Safe.t -> 'a
val field_opt : string -> 'a json_parser -> Yojson.Safe.t -> 'a option
val field : string -> 'a json_parser -> Yojson.Safe.t -> 'a

val nested_field_opt :
  string list -> 'a json_parser -> Yojson.Safe.t -> 'a option

val nested_field : string list -> 'a json_parser -> Yojson.Safe.t -> 'a
