
type category

val make : flag:string -> prefix:string -> (category * (string -> unit))

val get_categories : unit -> string list

val set_enabled : category -> bool -> unit

val get_enabled : category -> bool

val try_set_enabled : string -> bool -> bool

