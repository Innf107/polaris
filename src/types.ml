open Syntax
open Syntax.Renamed
open Util

type type_error = UnableToUnify of ty * ty

exception TypeError of loc * type_error

let typecheck exprs = todo __LOC__
