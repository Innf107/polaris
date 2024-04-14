open Polaris

exception NonexistentField of string * Yojson.Safe.t
exception NonObjectDereference of string * Yojson.Safe.t
exception WrongTypeAsserted of string * Yojson.Safe.t
exception ParseError

type 'a json_parser = {
  name : string;
  parser : Yojson.Safe.t -> 'a;
}

let make_parser_exn name parser = { name; parser }

let make_parser name parser =
  make_parser_exn name
    begin
      fun json ->
        match parser json with
        | Some value -> value
        | None -> raise ParseError
    end

let any = make_parser "any" (fun json -> Some json)

let bool =
  make_parser "bool" (function
    | `Bool value -> Some value
    | _ -> None)

let float =
  make_parser "float" (function
    | `Float value -> Some value
    | _ -> None)

let int =
  make_parser "int" (function
    | `Int value -> Some value
    | _ -> None)

let null =
  make_parser "null" (function
    | `Null -> Some ()
    | _ -> None)

let string =
  make_parser "string" (function
    | `String value -> Some value
    | _ -> None)

let list inner_parser =
  make_parser
    ("List(" ^ inner_parser.name ^ ")")
    (function
      | `List values -> begin
          try
            let values = List.map inner_parser.parser values in
            Some values
          with
          | ParseError -> None
        end
      | _ -> None)

let assoc inner_parser =
  make_parser
    ("Assoc(" ^ inner_parser.name ^ ")")
    (function
      | `Assoc values -> begin
          try
            let values =
              List.map
                (fun (key, value) -> (key, inner_parser.parser value))
                values
            in
            Some values
          with
          | ParseError -> None
        end
      | _ -> None)

let assert_json : 'a json_parser -> Yojson.Safe.t -> 'a =
 fun json_parser json ->
  match json_parser.parser json with
  | value -> value
  | exception ParseError -> raise (WrongTypeAsserted (json_parser.name, json))

let coerce_json : 'a json_parser -> Yojson.Safe.t -> 'a option =
 fun json_parser json ->
  match json_parser.parser json with
  | value -> Some value
  | exception ParseError -> None

let field_opt field json_type = function
  | `Assoc fields -> begin
      match List.find_opt (fun (key, _) -> key = field) fields with
      | None -> None
      | Some (_, value) -> Some (assert_json json_type value)
    end
  | _ -> None

let field field json_type = function
  | `Assoc _ as json -> begin
      match field_opt field json_type json with
      | Some result -> result
      | None -> raise (NonexistentField (field, json))
    end
  | json -> raise (NonObjectDereference (field, json))

let rec nested_field fields json_type json =
  match fields with
  | [] -> assert_json json_type json
  | field :: fields -> (
      match json with
      | `Assoc json_fields -> begin
          match List.find_opt (fun (key, _) -> key = field) json_fields with
          | Some (_, result) -> nested_field fields json_type result
          | None -> raise (NonexistentField (field, json))
        end
      | _ -> raise (NonObjectDereference (field, json)))

let nested_field_opt fields json_type json =
  try Some (nested_field fields json_type json) with
  | NonexistentField _
  | NonObjectDereference _ ->
      None

let () =
  Printexc.register_printer (function
    | NonexistentField (field, json) ->
        Some
          ("JSON assertion failure: Nonexistant field '" ^ field ^ "' in JSON: "
         ^ Yojson.Safe.show json)
    | NonObjectDereference (field, json) ->
        Some
          ("JSON assertion failure: Trying to dereference field '" ^ field
         ^ "' of non-object: " ^ Yojson.Safe.show json)
    | WrongTypeAsserted (ty, json) ->
        Some
          ("JSON assertion failure: Asserted wrong type '" ^ ty ^ "' for JSON: "
         ^ Yojson.Safe.show json)
    | _ -> None)
