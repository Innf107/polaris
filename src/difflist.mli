(** Efficient list builder *)
type 'a t

val empty : 'a t

(* O(1) *)
val of_list : 'a list -> 'a t

(* O(n) *)
val to_list : 'a t -> 'a list

(* O(n) *)
val to_seq : 'a t -> 'a Seq.t

(* O(n), but independent of the size of the appended list *)
val append_to_list : 'a t -> 'a list -> 'a list

(* O(1) *)
val append : 'a t -> 'a t -> 'a t

(* O(m), where `n` is the number of difflists*)
val concat : 'a t list -> 'a t

(* O(1) Adds a single element to the left *)
val cons : 'a -> 'a t -> 'a t

(* O(1) Adds a single element to the right *)
val snoc : 'a t -> 'a -> 'a t

(* O(n) *)
val iter : ('a -> unit) -> 'a t -> unit

(* O(n) *)
val iteri : (int -> 'a -> unit) -> 'a t -> unit

(* O(n) *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(* O(n) *)
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
 