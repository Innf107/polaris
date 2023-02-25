
type t = {
    loc : Polaris.Loc.t;
    severity : [ `Error | `Warning | `Information | `Hint ];
    source : string;
    message : string
}

val loc_to_json : Polaris.Loc.t -> Yojson.Safe.t

val to_json : t -> Yojson.Safe.t
