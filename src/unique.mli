type t

val fresh : unit -> t

val equal : t -> t -> bool

(* Provided to allow storing Unique.t's in a Map *)
val compare : t -> t -> int

val display : t -> string
