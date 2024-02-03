open Syntax
open Syntax.Typed

type t

val of_map : ty Map.Make(Name).t -> t

val apply : t -> name -> ty option

val apply_traversal : t -> unit Traversal.traversal

val apply_type : t -> ty -> ty
