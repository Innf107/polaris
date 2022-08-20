open Syntax
open Syntax.Renamed

type type_error = UnableToUnify of (ty * ty) * (ty * ty)
                                 (* ^           ^ full original types *)
                                 (* | specific mismatch               *)
                | Impredicative of (ty * ty) * (ty * ty)
                | OccursCheck of Unique.t * name * ty * ty * ty
                | WrongNumberOfArgs of ty list * ty list * ty * ty

exception TypeError of loc * type_error

(* Might throw 'TypeError' *)
val typecheck : expr list -> unit

