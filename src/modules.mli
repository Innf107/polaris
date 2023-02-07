open Syntax

val extract_import_paths_mod : Parsed.module_expr -> string list

val extract_import_paths : Parsed.expr -> string list


val build_export_map : Typed.header 
                    -> Typed.expr list 
                    -> Rename.RenameScope.t 
                    -> Types.global_env
                    -> Typed.module_exports
