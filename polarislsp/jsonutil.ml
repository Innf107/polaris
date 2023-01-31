
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

let rec string_of_json_type : type a. a json_type -> string = 
  function
  | Any -> "Any"
  | Assoc ty -> "Assoc(" ^ string_of_json_type ty ^ ")"
  | Bool -> "Bool"
  | Float -> "Float"
  | Int -> "Int"
  | List ty -> "List(" ^ string_of_json_type ty ^ ")"
  | Null -> "Null"
  | String -> "String"

let rec assert_json : type a. a json_type -> Yojson.Basic.t -> a =
  fun json_type json -> 
    match json_type, json with
    | Any, _ -> json
    | Bool, `Bool value -> value
    | Float, `Float value -> value
    | Int, `Int value -> value
    | String, `String value -> value 
    | Assoc inner_type, `Assoc fields ->
      List.map (fun (key, value) -> (key, assert_json inner_type value)) fields
    | List inner_type, `List values ->
      List.map (assert_json inner_type) values
    | Null, `Null -> ()
    | _ -> raise (WrongTypeAsserted (string_of_json_type json_type, json))

let coerce_json : type a. a json_type -> Yojson.Basic.t -> a option =
  fun json_type json ->
    try
      Some (assert_json json_type json)
    with
      WrongTypeAsserted _ -> None

let field_opt field json_type = function
  | `Assoc fields ->
    begin match List.find_opt (fun (key, _) -> key = field) fields with
    | None -> None
    | Some (_, value) -> Some (assert_json json_type value)
    end
  | _ -> None
    
let field field json_type = function
  | (`Assoc _) as json -> 
    begin match field_opt field json_type json with
    | Some result -> result
    | None -> raise (NonexistantField (field, json))
    end
  | json -> raise (NonObjectDereference (field, json))
    
let rec nested_field fields json_type json = match fields with
    | [] -> assert_json json_type json
    | (field :: fields) ->
      match json with
      | `Assoc json_fields ->
        begin match List.find_opt (fun (key, _) -> key = field) json_fields with
        | Some (_, result) -> nested_field fields json_type result
        | None -> raise (NonexistantField (field, json))
        end
      | _ -> raise (NonObjectDereference (field, json))

let nested_field_opt fields json_type json =
  try 
    Some(nested_field fields json_type json)
  with
  | NonexistantField _
  | NonObjectDereference _ -> None
