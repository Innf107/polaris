open Syntax

val extract_import_paths : string list Parsed.Traversal.traversal

val build_export_map :
  Typed.header ->
  Typed.expr list ->
  Rename.RenameScope.t ->
  Types.global_env ->
  Typed.module_exports
