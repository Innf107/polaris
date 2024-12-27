(* This needs to be defined here to avoid cyclical dependencies with Driver :/ *)
type module_error =
  | ModuleStdlibFileNotFound of Loc.t * string
  | UnableToImportModule of {
      loc : Loc.t;
      filename : string;
      reason : string;
    }
exception ModuleError of module_error


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
  | ModuleError of module_error



val handle_errors : (t -> 'a) -> (unit -> 'a) -> 'a

val pretty_error :
  Errormessage.text_style -> (Loc.t option -> string -> 'a) -> t -> 'a

val as_exn : ('a, t) result -> 'a
