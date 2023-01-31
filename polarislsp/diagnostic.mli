
type t = {
    loc : Polaris.Loc.t;
    severity : [ `Error | `Warning | `Information | `Hint ];
    source : string;
    message : string
}

val to_json : t -> Yojson.Basic.t
