module Make (Key : Map.OrderedType) : sig
  type 'a t
  type key = Key.t

  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val add_list : (key * 'a) list -> 'a t -> 'a t
  val to_seq : 'a t -> (key * 'a) Seq.t
  val of_list : (key * 'a) list -> 'a t
  val of_seq : (key * 'a) Seq.t -> 'a t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val find_first : key -> 'a t -> 'a option
  val find : key -> 'a t -> 'a list
  val union : 'a t -> 'a t -> 'a t
  val update : key -> ('a list -> 'a list) -> 'a t -> 'a t
  val delete_first : key -> 'a t -> 'a t
end
