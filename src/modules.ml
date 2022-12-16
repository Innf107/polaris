open Syntax

let extract_import_paths_mod =
  Parsed.MExpr.collect_list begin function
  | Import (_, path) -> [path]
  | _ -> []
  end

let extract_import_paths =
  Parsed.Expr.collect_list begin function
    | LetModuleSeq (_, _, mexpr) -> extract_import_paths_mod mexpr
    | _ -> []
  end

let build_export_map header exprs rename_scope global_env = 
  let export_item_to_map_entry = function
    | Renamed.ExportVal (_, name) ->
      let name_entry = (Name.original_name name, name) in
      let ty_entry = match NameMap.find_opt name Types.(global_env.var_types) with
        | Some ty -> (name, ty)
        | None -> Util.panic __LOC__ ("Exported item without inferred global type: " ^ Name.pretty name)
      in
      (name_entry, ty_entry)
  in
  let exported_names_seq, exported_types_seq = Seq.split (Seq.map export_item_to_map_entry Renamed.(List.to_seq header.exports)) in
  Renamed.{ exported_names = StringMap.of_seq exported_names_seq; exported_types = NameMap.of_seq exported_types_seq }



