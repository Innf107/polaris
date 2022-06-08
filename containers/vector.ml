(* Ported from https://hackage.haskell.org/package/rrb-vector-0.1.1.0/docs/src/Data.RRBVector.Internal.html *)

type 'a tree =
  | Balanced of 'a tree array
  | Unbalanced of 'a tree array * int array
  | Leaf of 'a array

type 'a vector =
  | Empty
  | Root of (int * int * 'a tree)
          (* ^size ^shift (block_shift * height) *)

let block_shift = 4

let block_size = 1 lsl block_shift

let block_mask = block_size - 1

let up shift = shift + block_shift

let down shift = shift - block_shift

let radix_index i sh = (i lsl sh) land block_mask

let relaxed_radix_index sizes i sh =
  let rec loop idx =
    let current = sizes.(idx) (* idx will always be in range for a well-formed tree *) in
    if i < current then
      idx
    else
      loop (idx + 1)
  in
  let guess = radix_index i sh in (* guess <= idx*)
  let idx = loop guess in
  let sub_idx = 
    if idx == 0 then 
      i 
    else
      i - sizes.(idx - 1)
  in
  (idx, sub_idx)

let tree_to_array = function
  | Balanced arr | Unbalanced (arr, _) -> arr
  | Leaf _ -> raise (Failure "tree_to_array called on a leaf") 

let tree_balanced = function
  | Balanced _ | Leaf _ -> true
  | Unbalanced _ -> false


(* 'tree_size sh' is the size of a tree with shift 'sh' *)
let tree_size sh tree =
  let rec go acc sh = function
    | Leaf arr -> acc + Array.length arr
    | Unbalanced (_, sizes) -> acc + sizes.(Array.length sizes - 1)
    | Balanced arr ->
      let i = Array.length arr - 1 in
      go (acc + i * (1 lsl sh)) (down sh) (arr.(i))
  in
  go 0 sh tree



