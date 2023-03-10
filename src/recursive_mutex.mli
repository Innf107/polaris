
type t

val create : unit -> t

val lock : t -> unit

val unlock : t -> unit
