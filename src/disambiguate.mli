open Syntax
open Syntax.Typed

type builder

val builder : builder

val ty : ty -> builder -> builder

val unify_context : Types.unify_context -> builder -> builder

val unify_context_option : Types.unify_context option -> builder -> builder

val types : ty list -> builder -> builder

val pretty_type : builder -> ty -> string
