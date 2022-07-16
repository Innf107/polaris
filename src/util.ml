
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

let rec sequence_options = function
  | None :: _ -> None
  | Some(v) :: opts -> Option.map (fun x -> v :: x) (sequence_options opts) 
  | [] -> Some []

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
  match quiet_command "which" [path] with
  | Unix.WEXITED 0 -> true
  | _ -> false

let take n list =
  List.of_seq (Seq.take n (List.to_seq list))

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: xs -> last xs
(* O(n) *)
let rec extract : ('a -> bool) -> 'a list -> ('a * 'a list) option =
  fun pred xs -> match xs with
  | [] -> None
  | x :: xs -> 
    if pred x then
      Some (x, xs)
    else
      match extract pred xs with
      | None -> None
      | Some (y, ys) -> Some(y, x::ys)
