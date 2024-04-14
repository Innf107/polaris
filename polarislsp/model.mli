open Polaris.Syntax
open Polaris.Syntax.Typed
module LocByOverlap : Map.OrderedType with type t = loc

(* This is a map where two locations are treated as equal as long as one
   is contained inside the other.
   Locations from different files are compared according to their file name *)
module LocOverlapMap : module type of Map.Make (LocByOverlap)

type model
type t = model

module ScopeCollection : sig
  type t
  type collector

  val new_collector : unit -> collector
  val registration_for : collector -> Polaris.Rename.scope_registration
  val collect : collector -> t
end

type hover_entry =
  | VarLike of name * ty
  | Subscript of string * ty
  | Variant of string * ty

val build : ScopeCollection.t -> expr list -> model

val find_hover_entry_at :
  file:string -> Lsp.position -> model -> (loc * hover_entry) option

val find_definition_at :
  file:string -> Lsp.position -> model -> (loc * loc) option

val find_scope_at_or_before : Lsp.position -> model -> Polaris.Rename.RenameScope.t