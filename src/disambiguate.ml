open Syntax
open Syntax.Typed

module UniqueMap = Map.Make(Unique)

type builder = {
  unifs : (int UniqueMap.t) StringMap.t;
  skolems : (int UniqueMap.t) StringMap.t;
}

let builder = {
  unifs = StringMap.empty;
  skolems = StringMap.empty;
}

let ty =
  let traversal = object
    inherit [builder] Traversal.traversal

    method! ty state = 
      let add_entry unique name inner_map =
        let inner_map = Option.value inner_map ~default:UniqueMap.empty in 
        let inner_map = UniqueMap.update unique begin 
          function
          (* Add a new entry with an increasing id *)
          | None -> Some (UniqueMap.cardinal inner_map)
          | Some x -> Some x
        end inner_map in
        Some inner_map
      in
      function
      | Unif (typeref, name) as ty -> 
        let state = { state with
          unifs = StringMap.update name.name (add_entry (Typeref.get_unique typeref) name) state.unifs
        } in
        ty, state
      | Skol (unique, _level, name) as ty ->
        let state = { state with
          skolems = StringMap.update name.name (add_entry unique name) state.skolems
        } in
        ty, state
      | ty -> ty, state
    end in 
    fun ty state ->
      let _, state = traversal#traverse_type state ty in
      state

let unify_context (type1, type2) builder =
  builder
  |> ty type1
  |> ty type2

let unify_context_option mcontext builder = 
  match mcontext with
  | None -> builder
  | Some context -> unify_context context builder

let types types disambiguate =
  List.fold_right ty types disambiguate

let rec pretty_type disambiguate =
  pretty_type_with {
    pretty_unif = begin fun typeref name -> 
      let suffix = match Option.bind (StringMap.find_opt name.name disambiguate.unifs)
                                    (UniqueMap.find_opt (Typeref.get_unique typeref)) with
      | None -> (Lazy.force default_pretty_type_config).pretty_unif typeref name
      | Some 0 -> ""
      | Some i -> string_of_int i
      in
      match Typeref.get typeref with
      | Unbound level ->
        "?" ^ Name.pretty name ^ suffix ^ (if Config.print_levels () then "[" ^ Typeref.pretty_level level ^ "]" else "")
      | Bound ty -> 
        if Config.print_subst_unif_vars () then
          "?" ^ Name.pretty name ^ suffix ^ "[= " ^ pretty_type disambiguate ty ^ "]"
        else
          pretty_type disambiguate ty
    end;
    pretty_skol = begin fun unique level name ->
      let suffix = match Option.bind (StringMap.find_opt name.name disambiguate.skolems)
                                    (UniqueMap.find_opt unique) with
      | None -> (Lazy.force default_pretty_type_config).pretty_skol unique level name
      | Some 0 -> ""
      | Some i -> string_of_int i
      in
      Name.pretty name ^ suffix
    end
  }
  