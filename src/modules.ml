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
  let export_item_to_map_entry = function
    | Renamed.ExportVal (_, name) ->
      let name_entry = (Name.original_name name, name) in
      let ty_entry = match NameMap.find_opt name Types.(global_env.var_types) with
        | Some ty -> (name, ty)
        | None -> Util.panic __LOC__ ("Exported variable without inferred global type: " ^ Name.pretty name)
      in
      (Some (name_entry, ty_entry), None)
    | Renamed.ExportData (_, name) ->
      let name_entry = (Name.original_name name, name) in      
      let definition_entry = match NameMap.find_opt name Types.(global_env.data_definitions) with
      | Some definition -> (name, definition)
      | None -> Util.panic __LOC__ ("Exported data type without definition: " ^ Name.pretty name)
      in
      (None, Some (name_entry, definition_entry))
  in
  let variable_entries, data_entries = Seq.unzip (Seq.map export_item_to_map_entry Renamed.(List.to_seq header.exports)) in

  let exported_variables_seq, exported_variable_types_seq = Seq.split (Seq.filter_map Fun.id variable_entries) in
  let exported_datas_seq, exported_data_efinitions_seq = Seq.split (Seq.filter_map Fun.id data_entries) in

  trace_exports (lazy ("Exported variables: [" ^ String.concat ", " (List.map (fun (x, ty) -> "(" ^ Name.pretty x ^ " : " ^ Renamed.pretty_type ty ^ ")") (List.of_seq exported_variable_types_seq)) ^ "]"));
  trace_exports (lazy ("Exported data constructors: [" ^ String.concat ", " (List.map (fun (_, x) -> Name.pretty x) (List.of_seq exported_datas_seq)) ^ "]"));

  Renamed.{ 
    exported_variables = StringMap.of_seq exported_variables_seq; 
    exported_variable_types = NameMap.of_seq exported_variable_types_seq;

    exported_datas = StringMap.of_seq exported_datas_seq;
    exported_data_definitions = NameMap.of_seq exported_data_efinitions_seq;
  }



