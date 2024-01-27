type binding

module Binding : sig 
  type t = binding
  val compare : t -> t -> int
end

type t =
  | Dictionary of binding
  | Meta of meta
  | Apply of t * t list

and meta

val make_binding : unit -> binding
val make_empty_meta : unit -> meta
val set_meta : meta -> t -> unit
val pretty : t -> string
val pretty_meta : meta -> string
val pretty_binding : binding -> string

val follow_metas : t -> t
(** Follows substituted top-level meta variables.
    The intent here is that pattern matching on `follow_metas evidence` should behave just like
    matching on evidence directly as if it didn't contain a top-level meta variable.

    THIS ONLY WORKS FOR TOP-LEVEL META VARIABLES!
    Any deep pattern matching on evidence values needs to be split into separate match expressions that each call follow_metas *)
