open Syntax

exception ParseError of loc * string

type specific_parse_error = Parserprelude.specific_parse_error
exception SpecificParseError of specific_parse_error

type driver_options = {
  filename : string;
  argv : string list;
  print_ast : bool;
  print_renamed : bool;
  print_tokens : bool;
}

val run : driver_options -> Lexing.lexbuf -> unit

val run_env : driver_options 
           -> Lexing.lexbuf 
           -> Eval.eval_env 
           -> Rename.RenameScope.t
           -> ?check_or_infer_top_level : [`Check | `Infer]
           -> Types.global_env
           -> Eval.value * Eval.eval_env * Rename.RenameScope.t * Types.global_env

val parse_rename_typecheck : driver_options 
                          -> Lexing.lexbuf
                          -> Rename.RenameScope.t
                          -> ?check_or_infer_top_level : [`Check | `Infer]
                          -> Types.global_env
                          -> Typed.header * Typed.expr list * Rename.RenameScope.t * Types.global_env


