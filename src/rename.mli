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
  | NonClassInConstraint of Renamed.ty * loc
  | NonClassInInstance of name * loc
  | ClassMethodMismatch of {
      class_name : name;
      missing : string list;
      invalid : string list;
      loc : loc;
    }
  | WrongNumberOfClassArgs of {
      class_name : name;
      expected : int;
      actual : int;
      loc : loc;
    }

exception RenameError of rename_error

module FilePathMap : module type of Map.Make (String)
module RenameMap : module type of Map.Make (String)

module RenameScope : sig
  type t = {
    level : Typeref.level;
    variables : (name * loc) RenameMap.t;
    module_vars : (name * t) RenameMap.t;
    ty_vars : name RenameMap.t;
    (* Polaris does not have a Haskell-style kind system, so we
       just check that type constructors are always fully applied in the renamer. *)
    ty_constructors :
      (name * name list * Typeref.level * type_constructor_sort) RenameMap.t;
    type_aliases : (name list * Renamed.ty) NameMap.t;
    underlying_data_constructor_implementations : Renamed.ty NameMap.t;
    data_constructors : (name * data_constructor_sort) RenameMap.t;
    type_classes : (name list * (name * Renamed.ty) StringMap.t) NameMap.t;
  }

  val empty : t

  val underlying_data_implementation : name -> t -> Renamed.ty
end

val rename_scope :
  (Typed.module_exports * Typed.expr list) FilePathMap.t ->
  RenameScope.t ->
  Parsed.header ->
  Parsed.expr list ->
  Renamed.header * Renamed.expr list * RenameScope.t
