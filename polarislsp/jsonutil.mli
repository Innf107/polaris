
exception NonexistantField of string * Yojson.Basic.t
exception NonObjectDereference of string * Yojson.Basic.t
exception WrongTypeAsserted of string * Yojson.Basic.t

type _ json_type =
    | Any    : Yojson.Basic.t json_type
    | Bool   : bool json_type
    | Float  : float json_type
    | Int    : int json_type
    | Null   : unit json_type
    | String : string json_type
    | List   : 'a json_type -> 'a list json_type
    | Assoc  : 'a json_type -> (string * 'a) list json_type

val coerce_json : 'a json_type -> Yojson.Basic.t -> 'a option

val assert_json : 'a json_type -> Yojson.Basic.t -> 'a

val field_opt : string -> 'a json_type -> Yojson.Basic.t -> 'a option

val field : string -> 'a json_type -> Yojson.Basic.t -> 'a

val nested_field_opt : string list -> 'a json_type -> Yojson.Basic.t -> 'a option
val nested_field : string list -> 'a json_type -> Yojson.Basic.t -> 'a
