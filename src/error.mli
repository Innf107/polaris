
type t = 
  | Panic of string
  | TODO of string
  | LexError of Lexer.lex_error
  | ParseError of Loc.t * string
  | SpecificParseError of Parserprelude.specific_parse_error 
  | SysError of string
  | RenameError of Rename.rename_error
  | TypeError of Loc.t * Types.type_error
  | EvalError of Eval.eval_error

val handle_errors : (t -> 'a) -> (unit -> 'a) -> 'a

val pretty_error : Errormessage.text_style -> (Loc.t option -> string -> 'a) -> t -> 'a

