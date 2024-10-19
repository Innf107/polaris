open Syntax

type pattern_error =
  | ListWithoutNil
  | ListWithoutCons
  | ExceptionWithoutWildcard
  | NumWithoutWildcard
  | StringWithoutWildcard
  | BoolWithout of bool
  | VariantNonExhaustive of string list

exception PatternError of pattern_error

val check_exhaustiveness_and_close_variants :
  close_variant:(Typed.ty -> unit) -> Typed.pattern list -> unit

type path_segment =
  | List
  | Tuple of int
  | Variant of string * int

type path = path_segment list

val check_variant_refutability : Typed.pattern -> (path * string) option
(** Check if a pattern is irrefutable except for a single variant pattern. 
  In that case, type inference can assume that this variant does not occur in further match cases    
*)

type refinement

val uncovered : refinement
val extend_refinement : refinement -> Renamed.pattern -> refinement

val refine :
  normalize_unif:(Renamed.ty -> Renamed.ty) ->
  unify:(Renamed.ty -> Renamed.ty -> unit) ->
  fresh_unif:(unit -> Renamed.ty) ->
  refine_variant:(Renamed.ty -> Renamed.ty -> string list -> unit) ->
  refinement ->
  Renamed.ty ->
  Renamed.ty * [ `FullyCovered | `NotYetCovered ]
