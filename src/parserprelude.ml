open Syntax

exception ParseError of loc * string

type specific_parse_error = MismatchedLetName of loc * string * string

exception SpecificParseError of specific_parse_error

let make_function sort domain codomain = match sort with
  | `Function -> Parsed.Fun (domain, codomain)
  | `Constraint ->
      List.fold_right (fun x r -> Parsed.Constraint (x, r)) domain codomain
