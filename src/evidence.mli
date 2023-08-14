type source =
    | Dictionary of Unique.t

type target =
    | BoundDictionary of source option ref

val make_source : unit -> source

val make_empty_target : unit -> target
val fill_target : target -> source -> unit


val display_source : source -> string
val display_target : target -> string
