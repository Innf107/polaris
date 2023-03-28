open Syntax
open Syntax.Renamed

type unify_context = (ty * ty)
type type_error = UnableToUnify of (ty * ty) * unify_context option
                                 (* ^           ^ full original types (None if there is no difference) *)
                                 (* | specific mismatch                                                *)
                | DifferentVariantConstrArgs of string * ty list * ty list * unify_context
                | MismatchedTyCon of name * name * unify_context option
                | Impredicative of (ty * ty) * unify_context option
                | OccursCheck of ty Typeref.t * name * ty * unify_context option
                | FunctionsWithDifferentArgCounts of ty list * ty list * unify_context
                | PassedIncorrectNumberOfArgsToFun of int * ty list * ty
                | IncorrectNumberOfArgsInLambda of int * ty list * ty
                | NonProgCallInPipe of expr
                | MissingRecordFields of (string * ty) list * (string * ty) list * unify_context
                | MissingVariantConstructors of (string * ty list) list * (string * ty list) list * unify_context
                | ArgCountMismatchInDefinition of name * ty list * int
                | NonFunTypeInLetRec of name * ty
                | CannotUnwrapNonData of ty
                | ValueRestriction of ty
                | SkolemUnifyEscape of ty * ty * ty * unify_context option
                                    (* ^    ^    ^ unified type
                                       |    | skolem
                                       | unif *)

exception TypeError of loc * type_error


type global_env = {
  var_types : Typed.ty NameMap.t;
  module_var_contents : global_env NameMap.t;
  data_definitions : (name list * Typed.ty) NameMap.t;
  type_aliases : (name list * Typed.ty) NameMap.t;
  ambient_level : Typeref.level;
}


(* Might throw 'TypeError' *)
val typecheck : [`Check | `Infer] -> Renamed.header -> Renamed.expr list -> global_env -> global_env * Typed.header * Typed.expr list

val empty_env : global_env
