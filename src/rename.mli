open Syntax

type rename_error =
  | VarNotFound of string * loc
  | ModuleVarNotFound of string * loc
  | TyVarNotFound of string * loc
  | TyConNotFound of string * loc
  | DataConNotFound of string * loc
  | TooManyArgsToDataConPattern of name * Renamed.pattern list * loc
  | SubscriptVarNotFound of string * loc
  | LetSeqInNonSeq of Parsed.expr * loc
  | SubModuleNotFound of string * loc
  | WrongNumberOfTyConArgs of name * int * Parsed.ty list * loc
  | NonExceptionInTry of name * loc
  | UnboundExportConstructor of string * loc
  | DuplicateKeyInRecordUpdate of string * loc

exception RenameError of rename_error

module FilePathMap : module type of Trie.String

module RenameScope : sig
  type t

  val empty : t

  val variables_in_scope : t -> name Seq.t
  val module_vars_in_scope : t -> name Seq.t
  val type_variables_in_scope : t -> name Seq.t
  val type_constructors_in_scope : t -> name Seq.t
  val data_constructors_in_scope : t -> name Seq.t
end

type scope_registration = {
  register_scope : Loc.position -> RenameScope.t -> unit;
  register_reset_scope : Loc.position -> RenameScope.t -> unit;
}

val ignored_scope_registration : scope_registration

val rename_scope :
  scope_registration ->
  (Typed.module_exports * Typed.expr list) FilePathMap.t ->
  RenameScope.t ->
  Parsed.header ->
  Parsed.expr list ->
  Renamed.header * Renamed.expr list * RenameScope.t
