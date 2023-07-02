type category = string

module CategoryMap = Map.Make (String)

let enabled_map : bool CategoryMap.t ref = ref CategoryMap.empty

let get_categories () =
  List.map (fun (cat, _) -> cat) (CategoryMap.bindings !enabled_map)

let set_enabled cat value =
  enabled_map := CategoryMap.add cat value !enabled_map

let try_set_enabled cat value =
  if CategoryMap.mem cat !enabled_map then begin
    enabled_map := CategoryMap.add cat value !enabled_map;
    true
  end
  else false

let get_enabled cat =
  Option.value ~default:false (CategoryMap.find_opt cat !enabled_map)

let make ~flag ~prefix =
  enabled_map := CategoryMap.add flag false !enabled_map;
  ( flag,
    fun msg ->
      if get_enabled flag then
        prerr_endline ("[" ^ prefix ^ "]: " ^ Lazy.force msg) )
