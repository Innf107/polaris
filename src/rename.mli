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

module FilePathMap : module type of Map.Make (String)

module RenameScope : sig
  type t

  val empty : t
end

val rename_scope :
  (Typed.module_exports * Typed.expr list) FilePathMap.t ->
  RenameScope.t ->
  Parsed.header ->
  Parsed.expr list ->
  Renamed.header * Renamed.expr list * RenameScope.t
