open Ppxlib

module LabelMap = Map.Make(String)

let instantiate_ttg : structure -> structure -> structure = 
  fun template instantiation -> 
    let collect_item_traversal = object
      inherit [structure_item list * core_type_desc LabelMap.t] Ast_traverse.fold as super

      method! structure_item item (items, types) = 
        let (items, types) = super#structure_item item (items, types) in
        match item with
        | { pstr_loc; 
            pstr_desc = Pstr_type (_, [{
              ptype_name;
              ptype_manifest=Some { ptyp_desc = ty; _ };
              _
            }])
          } 
          -> (items, LabelMap.add ptype_name.txt ty types)
        | item -> (item :: items, types)
    end in

    let instantiation_items, instantiation_types = collect_item_traversal#structure instantiation ([], LabelMap.empty) in

    let replace_traversal = object
      inherit Ast_traverse.map as super

      method! core_type_desc ty =
        let ty = super#core_type_desc ty in
        match ty with
        | Ptyp_constr (name, _) as ty -> 
          begin match name.txt with
          Lident label -> begin match LabelMap.find_opt label instantiation_types with
            | Some replacement_ty_desc -> replacement_ty_desc
            | None -> ty
            end
          | _ -> ty
          end
        | ty -> ty
    end in

    instantiation_items @ replace_traversal#structure template

let ttg_impl : structure -> structure 
  = fun structure -> 
    let separate_template_modules = function
      | { pstr_loc; pstr_desc=Pstr_module template_mod } as item -> 
        let is_template_attribute attr = attr.attr_name.txt = "ttg_template" in
        if List.exists is_template_attribute template_mod.pmb_attributes then

          match template_mod.pmb_name.txt with
          | Some name -> 
            begin match template_mod.pmb_expr with
            | { pmod_desc = Pmod_structure structure; _ }
              -> 
                Either.Left (name, structure)
            | { pmod_loc=loc; _ } -> 
              Either.Right[%stri [%error "ttg templates need to be named structure literals"]]
          end
          | None -> 
            let loc = template_mod.pmb_name.loc in
            Either.Right [%stri [%error "ttg_template needs to be used with a named module"]]
        else
          Either.Right item
      | item -> Either.Right item
    in
    let template_modules, new_structure = List.partition_map separate_template_modules structure in

    let template_modules = LabelMap.of_seq (List.to_seq template_modules) in

    let replace_template = function
      | { pstr_loc; pstr_desc=Pstr_module replacement_mod } as item ->
        let is_pass_attribute attr = attr.attr_name.txt = "ttg_pass" in
        if List.exists is_pass_attribute replacement_mod.pmb_attributes then
          match replacement_mod.pmb_expr with
          | { 
              pmod_desc = Pmod_apply (
                { pmod_desc = Pmod_ident { txt = Lident template_name; loc }; 
                  _ 
                }, 
                { pmod_desc = Pmod_structure instantiation;
                  _
                }); 
              pmod_loc; 
              _ 
            } ->
            begin match LabelMap.find_opt template_name template_modules with
            | Some template_expr -> 
              let structure = instantiate_ttg template_expr instantiation in
              { pstr_loc; 
                pstr_desc = Pstr_module 
                  { pmb_expr = 
                    { pmod_desc = Pmod_structure structure; 
                      pmod_loc = replacement_mod.pmb_loc; 
                      pmod_attributes = [] 
                    }; 
                    pmb_name = replacement_mod.pmb_name;
                    pmb_loc = replacement_mod.pmb_loc;
                    pmb_attributes = replacement_mod.pmb_attributes
                  } 
              }
            | None -> [%stri [%error "ttg_pass: Unkonwn template module"]]
            end
          | { pmod_loc = loc; _ } ->
            [%stri [%error "Modules with a ttg_pass attribute need to contain a functor application with a non-qualified name from the same module and a structure literal"]]
        else
          item
      | item -> item
    in
    List.map replace_template new_structure

let () =
  Driver.register_transformation
    ~impl: ttg_impl
    "trees_that_grow"
