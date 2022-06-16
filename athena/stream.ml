
type 'a t = Eof | Cons of 'a * 'a t lazy_t

let rec of_in_channel in_chan =
  match In_channel.input_char in_chan with
  | None -> In_channel.close in_chan; Eof
  | Some(c) -> Cons (c, lazy (of_in_channel in_chan))

let rec of_iter f =
  match f () with
  | None -> Eof
  | Some(c) -> Cons(c, lazy (of_iter f))

let rec of_list = function
  | [] -> Eof
  | (x :: xs) -> Cons (x, lazy (of_list xs))

let rec map f = function
  | Eof -> Eof
  | Cons (x, stream) -> Cons(f x, Lazy.map (map f) stream)

let next = function
  | Eof -> None
  | Cons (x, s) -> Some (x, Lazy.force s)

let rec collect = function
  | Eof -> []
  | Cons (x, s) -> x :: collect (Lazy.force s)
