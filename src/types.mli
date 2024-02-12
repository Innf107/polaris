open Syntax
open Syntax.Renamed

type unify_context = ty * ty


type given_constraints


val export_instances : given_constraints -> Obj.t NameMap.t

type type_error =
  | UnableToUnify of (ty * ty) * unify_context option
  (* ^           ^ full original types (None if there is no difference) *)
  (* | specific mismatch                                                *)
  | DifferentVariantConstrArgs of string * ty list * ty list * unify_context
  | MismatchedTyCon of name * name * unify_context option
  | Impredicative of (ty * ty) * unify_context option
  | OccursCheck of ty Typeref.t * name * ty * unify_context option
  | FunctionsWithDifferentArgCounts of ty list * ty list * unify_context
  | PassedIncorrectNumberOfArgsToFun of int * ty list * ty
  | IncorrectNumberOfArgsInLambda of int * ty list * ty
  | MissingRecordFields of
      (string * ty) list * (string * ty) list * unify_context
  | MissingVariantConstructors of
      (string * ty list) list * (string * ty list) list * unify_context
  | ArgCountMismatchInDefinition of name * ty list * int
  | NonFunTypeInLetRec of name * ty
  | CannotUnwrapNonData of ty
  | ValueRestriction of ty
  | SkolemUnifyEscape of ty * ty * ty * unify_context option
    (* ^    ^    ^ unified type
       |    | skolem
       | unif *)
  | DataConUnifyEscape of ty * name * ty * unify_context option
  | IncorrectNumberOfExceptionArgs of name * int * ty list
  | PatternError of Pattern.pattern_error
  | MissingInstance of class_constraint
  | TupleLiteralOfWrongLength of int * ty array
  | NonProgramArgument of ty
  | NonInterpolatable of ty
  | AmbiguousClassConstraint of class_constraint * (name list * ty list) list

type errors

type global_env = {
  var_types : Typed.ty NameMap.t;
  module_var_contents : global_env NameMap.t;
  data_definitions : (name list * Typed.ty) NameMap.t;
  type_aliases : (name list * Typed.ty) NameMap.t;
  ambient_level : Typeref.level;
  exception_definitions : ty list NameMap.t;
  type_classes : name list NameMap.t;
  given_constraints : given_constraints;
}

val typecheck :
  [ `Check | `Infer ] ->
  Renamed.header ->
  Renamed.expr list ->
  global_env ->
  (global_env * Typed.header * Typed.expr list, (loc * type_error) list) These.t

val empty_env : global_env
