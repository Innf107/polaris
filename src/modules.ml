open Syntax

let _export_category, trace_exports = Trace.make ~flag:"exports" ~prefix:"Exports" 


let extract_import_paths = object
    inherit [string list] Parsed.Traversal.traversal

    method! module_expr state = function
    | Import (_, path) as mod_expr ->
      mod_expr, path :: state
    | mod_expr -> mod_expr, state
  end

let build_export_map header exprs rename_scope global_env =
  let export_entry = function
  | Typed.ExportVal (_, name) ->
    let name_entry = (Name.original_name name, name) in
    let ty_entry = match NameMap.find_opt name Types.(global_env.var_types) with
      | Some ty -> (name, ty)
      | None -> Util.panic __LOC__ ("Exported variable without inferred global type: " ^ Name.pretty name)
    in
    `Val (name_entry, ty_entry)
  | Typed.ExportConstructor ((_, `Type), name) ->
    begin match NameMap.find_opt name Types.(global_env.data_definitions) with
    | Some (params, underlying) ->
      let tycon_entry = (name.name, (name, List.length params, DataConSort)) in
      let data_con_entry = (name, (params, underlying)) in
      `DataCon (tycon_entry, data_con_entry)
      | None -> begin match NameMap.find_opt name Types.(global_env.type_aliases) with
        | None -> Util.panic __LOC__ ("Exported type not found in build_export_map: " ^ Name.pretty name)
        | Some (params, underlying) ->
          let tycon_entry = (name.name, (name, List.length params, TypeAliasSort)) in
          let data_con_entry = (name, (params, underlying)) in
          `TypeAlias (tycon_entry, data_con_entry)
        end
    end
  
  | Typed.ExportConstructor ((_, `Exception), name) ->
    begin match NameMap.find_opt name Types.(global_env.exception_definitions) with
    | Some (params) ->
      `Exception (name, params)
    | None -> Util.panic __LOC__ ("Exception not found in build_export_map: " ^ Name.pretty name)
    end
  in

  let entries = List.map export_entry Typed.(header.exports) in

  let exported_variables, exported_variable_types = 
    List.split (List.filter_map (function `Val x -> Some x | _ -> None) entries)
  in

  let exported_ty_constrs_data, exported_data_definitions =
    List.split (List.filter_map (function `DataCon x -> Some x | _ -> None) entries)
  in

  let exported_ty_constrs_alias, exported_type_aliases = 
    List.split (List.filter_map (function `TypeAlias x -> Some x | _ -> None) entries)
  in

  let exported_exceptions =
    List.filter_map (function `Exception x -> Some x | _ -> None) entries
  in


  let exported_ty_constructors = exported_ty_constrs_data @ exported_ty_constrs_alias in

  trace_exports (lazy ("Exported variables: [" ^ String.concat ", " (List.map (fun (x, ty) -> "(" ^ Name.pretty x ^ " : " ^ Typed.pretty_type ty ^ ")") exported_variable_types) ^ "]"));
  trace_exports (lazy ("Exported type constructors: [" ^ String.concat ", " (List.map (fun (_, (x, _, _)) -> Name.pretty x) exported_ty_constructors) ^ "]"));
  trace_exports (lazy ("Exported exceptions: [" ^ String.concat ", " (List.map (fun (x, _) -> Name.pretty x) exported_exceptions) ^ "]"));

  Typed.{ 
    exported_variables = StringMap.of_seq (List.to_seq exported_variables); 
    exported_variable_types = NameMap.of_seq (List.to_seq exported_variable_types); 

    exported_ty_constructors = StringMap.of_seq (List.to_seq exported_ty_constructors);
    exported_data_definitions = NameMap.of_seq (List.to_seq exported_data_definitions);
    exported_exceptions = NameMap.of_seq (List.to_seq exported_exceptions);

    exported_type_aliases = NameMap.of_seq (List.to_seq exported_type_aliases);

    exported_type_classes = Util.todo __LOC__
  }



