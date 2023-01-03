open Syntax

let _export_category, trace_exports = Trace.make ~flag:"exports" ~prefix:"Exports" 


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
  let variable_to_map_entry = function
  | Renamed.ExportVal (_, name) ->
    let name_entry = (Name.original_name name, name) in
    let ty_entry = match NameMap.find_opt name Types.(global_env.var_types) with
      | Some ty -> (name, ty)
      | None -> Util.panic __LOC__ ("Exported variable without inferred global type: " ^ Name.pretty name)
    in
    Some (name_entry, ty_entry)
  | _ -> None
  in
  let data_con_to_map_entry = function
  | Renamed.ExportType (_, name) ->
    begin match NameMap.find_opt name Types.(global_env.data_definitions) with
    | None -> None
    | Some (params, underlying) ->
      let tycon_entry = (name.name, (name, List.length params, DataConSort)) in
      let data_con_entry = (name, (params, underlying)) in
      Some (tycon_entry, data_con_entry)
    end
  | _ -> None
  in
  let type_alias_to_map_entry = function
  | Renamed.ExportType (_, name) ->
    begin match NameMap.find_opt name Types.(global_env.type_aliases) with
    | None -> None
    | Some (params, underlying) ->
      let tycon_entry = (name.name, (name, List.length params, TypeAliasSort)) in
      let data_con_entry = (name, (params, underlying)) in
      Some (tycon_entry, data_con_entry)
    end
  | _ -> None
  in

  let exported_variables, exported_variable_types = List.split (List.filter_map variable_to_map_entry Renamed.(header.exports)) in

  let exported_ty_constrs_data, exported_data_definitions = List.split (List.filter_map data_con_to_map_entry header.exports) in

  let exported_ty_constrs_alias, exported_type_aliases = List.split (List.filter_map type_alias_to_map_entry header.exports) in

  let exported_ty_constructors = exported_ty_constrs_data @ exported_ty_constrs_alias in

  trace_exports (lazy ("Exported variables: [" ^ String.concat ", " (List.map (fun (x, ty) -> "(" ^ Name.pretty x ^ " : " ^ Renamed.pretty_type ty ^ ")") exported_variable_types) ^ "]"));
  trace_exports (lazy ("Exported type constructors: [" ^ String.concat ", " (List.map (fun (_, (x, _, _)) -> Name.pretty x) exported_ty_constructors) ^ "]"));

  Renamed.{ 
    exported_variables = StringMap.of_seq (List.to_seq exported_variables); 
    exported_variable_types = NameMap.of_seq (List.to_seq exported_variable_types); 

    exported_ty_constructors = StringMap.of_seq (List.to_seq exported_ty_constructors);
    exported_data_definitions = NameMap.of_seq (List.to_seq exported_data_definitions);

    exported_type_aliases = NameMap.of_seq (List.to_seq exported_type_aliases);
  }



