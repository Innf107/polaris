
(** Immutable arrays supporting O(1) slices
*)
type 'a t

val empty : 'a t

(** Create a vector from a mutable array by copying the elements
    O(n) *)
val freeze : 'a array -> 'a t

(** Create a vector from an underlying mutable array without a copy.
    This is only safe if the underlying array is not mutated afterwards! 
    O(1)*)
val unsafe_freeze : 'a array -> 'a t

(** Create a mutable array from a vector
    O(n) *)
val thaw : 'a t -> 'a array

(** Create a mutable array from a vector avoiding a copy if possible.
    This is only safe if the vector is not used afterwards.
    O(1) for vectors represented by a full array (e.g. ones created by freeze, make or copy)
    O(n) for slices *)
val unsafe_thaw : 'a t -> 'a array

(** Copy the underlying elements into a new vector.
    This allows the underlying vector of a slice to be garbage collected. 
    O(n) *)
val copy : 'a t -> 'a t

(** O(1) *)
val sub : 'a t -> int -> int -> 'a t

(** O(1) *)
val length : 'a t -> int

(** O(1) *)
val get : 'a t -> int -> 'a

(** O(n) *)
val make : int -> 'a -> 'a t

(** O(n) *)
val init : int -> (int -> 'a) -> 'a t

(** O(n * m) *)
val make_matrix : int -> int -> 'a -> 'a t t

(** O(n + m) *)
val append : 'a t -> 'a t -> 'a t

(** O(n) where n is the size of the result *)
val concat : 'a t list -> 'a t

(** O(n) *)
val to_list : 'a t -> 'a list

(** O(n) *)
val of_list : 'a list -> 'a t

(** O(n) *)
val iter : ('a -> unit) -> 'a t -> unit

(** O(n) *)
val iteri : (int -> 'a -> unit) -> 'a t -> unit

(** O(n) *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** O(n) *)
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

(** O(n) *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(** O(n) *)
val fold_left_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t

(** O(n) *)
val fold_right : ('b -> 'a -> 'a) -> 'b t -> 'a -> 'a

(** O(n) *)
val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit

(** O(n) *)
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

(** O(n) *)
val for_all : ('a -> bool) -> 'a t -> bool

(** O(n) *)
val exists : ('a -> bool) -> 'a t -> bool

(** O(n) *)
val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

(** O(n) *)
val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

(** O(n) *)
val mem : 'a -> 'a t -> bool

(** O(n) *)
val memq : 'a -> 'a t -> bool

(** O(n) *)
val find_opt : ('a -> bool) -> 'a t -> 'a option

(** O(n) *)
val find_map : ('a -> 'b option) -> 'a t -> 'b option

(** O(n) *)
val split : ('a * 'b) t -> 'a t * 'b t

(** O(n) *)
val combine : 'a t -> 'b t -> ('a * 'b) t

(** O(n log n) *)
val sort : ('a -> 'a -> int) -> 'a t -> 'a t

(** O(n log n) *)
val stable_sort : ('a -> 'a -> int) -> 'a t -> 'a t

(** O(n log n) *)
val fast_sort : ('a -> 'a -> int) -> 'a t -> 'a t

(** O(n) *)
val to_seq : 'a t -> 'a Seq.t

(** O(n) *)
val to_seqi : 'a t -> (int * 'a) Seq.t

(** O(n) *)
val of_seq : 'a Seq.t -> 'a t