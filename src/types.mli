open Syntax
open Renamed

module Subst : sig
    type t
end

type tc_env

val typecheck : expr list -> ty

