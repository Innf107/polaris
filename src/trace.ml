type category = { mutable is_enabled : bool }

let categories : category Trie.String.t ref = ref Trie.String.empty
let get_categories () = List.of_seq (Trie.String.keys !categories)
let set_enabled category value = category.is_enabled <- value

let try_set_enabled category_name value =
  match Trie.String.find_opt category_name !categories with
  | None -> false
  | Some category ->
      set_enabled category value;
      true

let get_enabled cat = cat.is_enabled

let make ~flag ~prefix =
  let category = { is_enabled = false } in
  categories := Trie.String.add flag category !categories;
  ( category,
    fun msg ->
      if get_enabled category then
        prerr_endline ("[" ^ prefix ^ "]: " ^ Lazy.force msg) )
