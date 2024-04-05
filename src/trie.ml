module type S = sig
  type key
  type +!'a t

  val empty : 'a t
  val singleton : key -> 'a -> 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find_subtrie : key -> 'a t -> 'a t option
  val find_opt : key -> 'a t -> 'a option
  val mem : key -> 'a t -> bool
  val length : 'a t -> int
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val of_seq : (key * 'a) Seq.t -> 'a t
  val to_seq : 'a t -> (key * 'a) Seq.t
  val bindings : 'a t -> (key * 'a) list
  val keys : 'a t -> key Seq.t
  val values : 'a t -> 'a Seq.t
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
end

module Make (Key : sig
  type t

  module Segment : Map.OrderedType

  type iterator

  val to_iterator : t -> iterator
  val next : iterator -> (Segment.t * iterator) option
end) : S with type key = Key.t = struct
  type key = Key.t

  module SegmentMap = Map.Make (Key.Segment)

  type 'a t = {
    children : 'a t SegmentMap.t;
    size : int;
    entry : (key * 'a) option;
  }

  let empty = { children = SegmentMap.empty; size = 0; entry = None }

  let rec singleton_iterator : type a. Key.iterator -> key -> a -> a t =
   fun iterator key value ->
    match Key.next iterator with
    | None ->
        { children = SegmentMap.empty; size = 1; entry = Some (key, value) }
    | Some (segment, iterator) ->
        let subtrie = singleton_iterator iterator key value in
        {
          size = 1;
          children = SegmentMap.singleton segment subtrie;
          entry = None;
        }

  (* TODO: compression*)
  let singleton : type a. key -> a -> a t =
   fun key value -> singleton_iterator (Key.to_iterator key) key value

  let rec add_iterator : type a. key -> Key.iterator -> a -> a t -> a t =
   fun key iterator value trie ->
    match Key.next iterator with
    | None -> { trie with entry = Some (key, value); size = trie.size + 1 }
    | Some (segment, iterator) ->
        let size_change = ref 0 in
        let update_entry = function
          | None ->
              size_change := 1;
              Some (singleton_iterator iterator key value)
          | Some child ->
              let new_child = add_iterator key iterator value child in
              size_change := new_child.size - child.size;
              Some new_child
        in
        let children = SegmentMap.update segment update_entry trie.children in
        { trie with children; size = trie.size + !size_change }

  let add key value trie = add_iterator key (Key.to_iterator key) value trie

  let rec find_subtrie_iterator iterator trie =
    match Key.next iterator with
    | None -> Some trie
    | Some (segment, iterator) -> begin
        match SegmentMap.find_opt segment trie.children with
        | None -> None
        | Some subtrie -> find_subtrie_iterator iterator subtrie
      end

  let find_subtrie key trie = find_subtrie_iterator (Key.to_iterator key) trie

  let find_opt key trie =
    match find_subtrie key trie with
    | None -> None
    | Some subtrie -> Option.map snd subtrie.entry

  let mem key trie = Option.is_some (find_opt key trie)
  let length trie = trie.size

  let rec map f trie =
    {
      trie with
      entry = Option.map (fun (key, value) -> (key, f value)) trie.entry;
      children = SegmentMap.map (map f) trie.children;
    }

  let rec mapi f trie =
    {
      trie with
      entry = Option.map (fun (key, value) -> (key, f key value)) trie.entry;
      children = SegmentMap.map (mapi f) trie.children;
    }

  let of_seq seq =
    Seq.fold_left (fun trie (key, value) -> add key value trie) empty seq

  let rec to_seq trie =
    let children =
      Seq.concat_map
        (fun (key, subtrie) -> to_seq subtrie)
        (SegmentMap.to_seq trie.children)
    in
    match trie.entry with
    | None -> children
    | Some entry -> fun () -> Seq.Cons (entry, children)

  let keys trie = Seq.map fst (to_seq trie)
  let values trie = Seq.map snd (to_seq trie)
  let bindings trie = List.of_seq (to_seq trie)

  let update : type a. key -> (a option -> a option) -> a t -> a t =
   fun key f trie ->
    let rec go iterator trie =
      match Key.next iterator with
      | None ->
          let entry =
            match f (Option.map snd trie.entry) with
            | Some value -> Some (key, value)
            | None -> None
          in
          let size =
            match (trie.entry, entry) with
            | None, None
            | Some _, Some _ ->
                trie.size
            | Some _, None -> trie.size - 1
            | None, Some _ -> trie.size + 1
          in
          { trie with entry; size }
      | Some (segment, iterator) -> begin
          match SegmentMap.find_opt segment trie.children with
          | None -> begin
              match f None with
              | None -> trie
              | Some value ->
                  let subtrie = singleton_iterator iterator key value in
                  {
                    trie with
                    children = SegmentMap.add segment subtrie trie.children;
                    size = trie.size + 1;
                  }
            end
          | Some subtrie ->
              let updated_subtrie = go iterator subtrie in
              (* TODO: this should probably use SegmentMap.update instead of lookup and then add? *)
              {
                trie with
                children = SegmentMap.add segment updated_subtrie trie.children;
                size = trie.size + updated_subtrie.size - subtrie.size;
              }
        end
    in
    go (Key.to_iterator key) trie

  let rec iter : type a. (key -> a -> unit) -> a t -> unit =
   fun f trie ->
    begin
      match trie.entry with
      | None -> ()
      | Some (key, value) -> f key value
    end;
    SegmentMap.iter (fun _key value -> iter f value) trie.children

  let fold : type a acc. (key -> a -> acc -> acc) -> a t -> acc -> acc =
   fun f trie initial ->
    let acc = ref initial in
    iter (fun key value -> acc := f key value !acc) trie;
    !acc
end

module String = Make (struct
  type t = string

  module Segment = Char

  type iterator = string * int

  let to_iterator str = (str, 0)

  let next (str, index) =
    if index < String.length str then Some (str.[index], (str, index + 1))
    else None
end)
