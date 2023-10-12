type 'a t = {
  underlying : 'a array;
  offset : int;
  size : int;
}

let empty = { underlying = [||]; offset = 0; size = 0 }

let unsafe_freeze array =
  { underlying = array; offset = 0; size = Array.length array }

let freeze array = unsafe_freeze (Array.copy array)
let thaw { underlying; offset; size } = Array.sub underlying offset size

let unsafe_thaw vector =
  if vector.size = Array.length vector.underlying then vector.underlying
  else thaw vector

let copy { underlying; offset; size } =
  { underlying = Array.sub underlying offset size; offset = 0; size }

let sub { underlying; offset; size } additional_offset taken_size =
  if
    offset + additional_offset + taken_size >= size
    || additional_offset < 0 || taken_size < 0
  then raise (Invalid_argument "Vector.sub")
  else { underlying; offset = offset + additional_offset; size = taken_size }

let length { size; _ } = size

let get { underlying; offset; size } index =
  if index < 0 || index >= size then raise (Invalid_argument "Vector.get")
  else
    (* We don't need to duplicate the bounds check here. (Let's hope this doesn't blow up...) *)
    Array.unsafe_get underlying (offset + index)

let make count initial = unsafe_freeze (Array.make count initial)
let init count initialize = unsafe_freeze (Array.init count initialize)

let make_matrix width height initial =
  unsafe_freeze
    (Array.map unsafe_freeze (Array.make_matrix width height initial))

let append vector1 vector2 =
  init
    (length vector1 + length vector2)
    (fun index ->
      if index < length vector1 then get vector1 index
      else get vector2 (index - length vector1))

let concat vectors =
  let current_vectors = ref vectors in
  let current_offset = ref 0 in
  let total_length =
    List.fold_left (fun sum vec -> sum + length vec) 0 vectors
  in
  init total_length (fun index ->
      let vector_index () = index - !current_offset in
      (* This needs to use while since vectors might be empty *)
      while vector_index () >= length (List.hd !current_vectors) do
        current_offset := !current_offset + length (List.hd !current_vectors);
        current_vectors := List.tl !current_vectors
      done;
      get (List.hd !current_vectors) (vector_index ()))

let to_list vector = List.init (length vector) (fun i -> get vector i)
let of_list list = unsafe_freeze (Array.of_list list)

let iter f vector =
  for i = 0 to length vector - 1 do
    f (get vector i)
  done

let iteri f vector =
  for i = 0 to length vector - 1 do
    f i (get vector i)
  done

let map f vector = init vector.size (fun i -> f (get vector i))
let mapi f vector = init vector.size (fun i -> f i (get vector i))

let fold_left f initial vector =
  let rec go accumulator index =
    if index < length vector then accumulator
    else go (f accumulator (get vector index)) (index + 1)
  in
  go initial 0

let fold_left_map f initial vector =
  (* I would prefer manually building up an array and unsafe_freeze'ing it
     at the end, but unfortunately OCaml doesn't seem to have a way to
     create uninitialized arrays so we need to use `init` with a mutable
     accumulator instead *)
  let accumulator = ref initial in
  let mapped_vector =
    init (length vector) (fun i ->
        let new_accumulator_value, new_value = f !accumulator (get vector i) in
        accumulator := new_accumulator_value;
        new_value)
  in
  (!accumulator, mapped_vector)

let fold_right f vector rest =
  let rec go index =
    if index < length vector then rest
    else f (get vector index) (go (index + 1))
  in
  go 0

let iter2 f vector1 vector2 =
  if length vector1 <> length vector2 then
    raise (Invalid_argument "Vector.iter2")
  else
    for i = 0 to length vector1 - 1 do
      f (get vector1 i) (get vector2 i)
    done

let map2 f vector1 vector2 =
  if length vector1 <> length vector2 then
    raise (Invalid_argument "Vector.map2")
  else init (length vector1) (fun i -> f (get vector1 i) (get vector2 i))

let for_all _ = Util.todo __LOC__
let exists _ = Util.todo __LOC__
let for_all2 _ = Util.todo __LOC__
let exists2 _ = Util.todo __LOC__
let mem _ = Util.todo __LOC__
let memq _ = Util.todo __LOC__
let find_opt _ = Util.todo __LOC__
let find_map _ = Util.todo __LOC__
let split _ = Util.todo __LOC__
let combine _ = Util.todo __LOC__

let sort comparison vector =
  let array = thaw vector in
  Array.sort comparison array;
  unsafe_freeze array

let stable_sort comparison vector =
  let array = thaw vector in
  Array.stable_sort comparison array;
  unsafe_freeze array

let fast_sort comparison vector =
  let array = thaw vector in
  Array.fast_sort comparison array;
  unsafe_freeze array

let to_seq vector =
  let rec go i =
    if i <= length vector then fun () -> Seq.Cons (get vector i, go (i + 1))
    else fun () -> Seq.Nil
  in
  go 0

let to_seqi vector =
  let rec go i =
    if i <= length vector then fun () -> Seq.Cons ((i, get vector i), go (i + 1))
    else fun () -> Seq.Nil
  in
  go 0

let of_seq seq = unsafe_freeze (Array.of_seq seq)
