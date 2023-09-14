exception TODO of string
exception Panic of string

(* Should be used as `panic __LOC__ "message"`*)
let panic loc msg = raise (Panic (loc ^ ": " ^ msg))

(* Should be used as `todo __LOC__` *)
let todo str = raise (TODO str)

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

let rec take_exact count list =
  match (count, list) with
  | 0, _ -> Some []
  | n, x :: xs -> Option.map (fun y -> x :: y) (take_exact (n - 1) xs)
  | _, [] -> None

let split_at_exact ix list =
  let rec go ix found rest =
    match (ix, found, rest) with
    | 0, xs, ys -> Some (List.rev xs, ys)
    | n, xs, y :: ys -> go (n - 1) (y :: xs) ys
    | n, xs, [] -> None
  in
  go ix [] list

let max z list = List.fold_left (fun r x -> if x > r then x else r) z list

let pad_right count padding str =
  str ^ String.make (count - String.length str) padding

type void = |

let absurd : void -> 'a 
  = function
  | _ -> .

let rec sequence_options = function
  | None :: _ -> None
  | Some v :: opts -> Option.map (fun x -> v :: x) (sequence_options opts)
  | [] -> Some []

let sequence_options_array arr =
  if Array.for_all Option.is_some arr then Some (Array.map Option.get arr)
  else None

let quiet_command cmd args =
  match Unix.fork () with
  | 0 ->
      let devnull = Unix.openfile "/dev/null" [] 0 in
      Unix.dup2 devnull Unix.stdin;
      Unix.dup2 devnull Unix.stdout;
      Unix.dup2 devnull Unix.stderr;
      Unix.execvp cmd (Array.of_list (cmd :: args))
  | pid ->
      let _, status = Unix.waitpid [] pid in
      status

let command_exists path =
  match quiet_command "which" [ path ] with
  | Unix.WEXITED 0 -> true
  | _ -> false

let take n list = List.of_seq (Seq.take n (List.to_seq list))

let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: xs -> last xs

(* O(n) *)
let rec extract : ('a -> bool) -> 'a list -> ('a * 'a list) option =
 fun pred xs ->
  match xs with
  | [] -> None
  | x :: xs -> (
      if pred x then Some (x, xs)
      else
        match extract pred xs with
        | None -> None
        | Some (y, ys) -> Some (y, x :: ys))

let ( << ) f g x = f (g x)

let path_relative_to : string -> string -> string =
 fun base_file path ->
  if Filename.is_relative path then
    Filename.concat (Filename.dirname base_file) path
  else path

let rec split3 = function
  | [] -> ([], [], [])
  | (x, y, z) :: rest ->
      let xs, ys, zs = split3 rest in
      (x :: xs, y :: ys, z :: zs)

let compose funs = List.fold_right (fun t r x -> t (r x)) funs (fun x -> x)

let abbreviate message =
  if String.length message <= 100 then message
  else String.sub message 0 100 ^ "..."

let rec replicate count x =
  match count with
  | count when count <= 0 -> []
  | count -> x :: replicate (count - 1) x

let async_promise : switch:Eio.Switch.t -> (unit -> 'a) -> 'a Eio.Promise.t =
 fun ~switch cont ->
  let promise, resolver = Eio.Promise.create () in
  Eio.Fiber.fork ~sw:switch (fun () ->
      let result = cont () in
      Eio.Promise.resolve resolver result);
  promise

let map_array_once f array =
  let mapped_at =
    Base.Array.find_mapi array ~f:(fun i x ->
        Option.map (fun y -> (i, x)) (f x))
  in
  match mapped_at with
  | None -> None
  | Some (index, new_value) ->
      let new_array = Array.copy array in
      new_array.(index) <- new_value;
      Some new_array

let map_array_at index f array =
  let new_array = Array.copy array in
  new_array.(index) <- f array.(index);
  new_array

let map_at index f list =
  List.mapi (fun i x -> if i = index then f x else x) list

let immutable_stable_sort compare array =
  let other = Array.copy array in
  Array.stable_sort compare other;
  other
