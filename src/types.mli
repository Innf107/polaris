open Syntax
open Syntax.Renamed

type type_error = UnableToUnify of ty * ty

exception TypeError of loc * type_error

(* Might throw 'TypeError' *)
val typecheck : expr list -> unit

