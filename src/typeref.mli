type level

val initial_top_level : level
val next_level : level -> level
val pretty_level : level -> string
val generalizable_level : ambient:level -> level -> bool
val escaping_level : ambient:level -> level -> bool
val unifiable_level : type_level:level -> unif_level:level -> bool

type 'a state =
  | Unbound of level
  | Bound of 'a

type 'a t

val make : level -> 'a t
val adjust_level : level -> 'a t -> unit
val get : 'a t -> 'a state
val set : 'a t -> 'a -> unit
val get_unique : 'a t -> Unique.t
val equal : 'a t -> 'a t -> bool
