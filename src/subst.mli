open Syntax.Renamed

type t

val make : unit -> t

val find : Unique.t -> t -> ty option

val apply : t -> ty -> ty

val add : Unique.t -> ty -> t -> unit
