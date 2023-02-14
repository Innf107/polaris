
type 'a t

val make : unit -> 'a t

val get : 'a t -> 'a option

val set : 'a t -> 'a -> unit

val get_unique : 'a t -> Unique.t

val equal : 'a t -> 'a t -> bool
