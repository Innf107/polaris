open Syntax

exception ParseError of loc * string

type specific_parse_error =
    | MismatchedLetName of loc * string * string

exception SpecificParseError of specific_parse_error
