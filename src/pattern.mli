open Syntax

type pattern_error = 
  | ListWithoutNil
  | ListWithoutCons
  | ExceptionWithoutWildcard
  | NumWithoutWildcard
  | StringWithoutWildcard
  | VariantNonExhaustive of string list

exception PatternError of pattern_error

val check_exhaustiveness_and_close_variants 
  :  close_variant:(Typed.ty -> unit)
  -> Typed.pattern list
  -> unit


type path_segment =
  | List
  | Tuple of int
  | Variant of string * int

type path = path_segment list

(** Check if a pattern is irrefutable except for a single variant pattern. 
  In that case, type inference can assume that this variant does not occur in further match cases    
*)
val check_variant_refutability : Typed.pattern -> (path * string) option