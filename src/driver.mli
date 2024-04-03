open Syntax

type driver_options = {
  filename : string;
  argv : string list;
  print_ast : bool;
  print_renamed : bool;
  print_tokens : bool;
}

val run :
  driver_options ->
  Sedlexing.lexbuf ->
  fs:Eio.Fs.dir Eio.Path.t ->
  mgr:Eio.Process.mgr ->
  (Eval.value, Error.t) result

val run_env :
  driver_options ->
  Sedlexing.lexbuf ->
  Eval.eval_env ->
  Rename.RenameScope.t ->
  ?check_or_infer_top_level:[ `Check | `Infer ] ->
  Types.global_env ->
  fs:Eio.Fs.dir Eio.Path.t ->
  mgr:Eio.Process.mgr ->
  ( Eval.value * Eval.eval_env * Rename.RenameScope.t * Types.global_env,
    Error.t )
  result

val parse_rename_typecheck :
  driver_options ->
  Sedlexing.lexbuf ->
  Rename.RenameScope.t ->
  ?check_or_infer_top_level:[ `Check | `Infer ] ->
  Types.global_env ->
  ( Typed.header * Typed.expr list * Rename.RenameScope.t * Types.global_env,
    Error.t )
  result
