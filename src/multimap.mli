
module Make(Key : Map.OrderedType) : sig
    type 'a t
    type key = Key.t

    val empty : 'a t

    val add : key -> 'a -> 'a t -> 'a t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t

    val to_seq : 'a t -> (key * 'a) Seq.t
    val of_seq : (key * 'a) Seq.t -> 'a t

    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val find : key -> 'a t -> 'a list

    val union : 'a t -> 'a t -> 'a t

    val update : key -> ('a list -> 'a list) -> 'a t -> 'a t
end
