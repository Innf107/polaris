
exception TODO of string * int * int * int
exception Panic of string

(* Should be used as `todo __POS__` *)
let todo (str, x, y, z) = raise (TODO (str, x, y ,z))

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

let rec take_exact count list = match count, list with
| 0, _ -> Some []
| n, (x::xs) -> Option.map (fun y -> x::y) (take_exact (n - 1) xs)
| _, [] -> None

let split_at_exact ix list =
  let rec go ix found rest = match ix, found, rest with
  | 0, xs, ys -> Some (List.rev xs, ys)
  | n, xs, (y::ys) -> go (n - 1) (y::xs) ys
  | n, xs, [] -> None
  in
  go ix [] list

let max z list =
  List.fold_left (fun r x -> if x > r then x else r) z list

let pad_right count padding str =
  str ^ String.make (count - String.length str) padding

type void

let absurd (_ : void) = raise (Panic "absurd: impossible argument")
