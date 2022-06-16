
type 'a t

val next : 'a t -> ('a * 'a t) option

val map : ('a -> 'b) -> 'a t -> 'b t

val collect : 'a t -> 'a list

(** Lazily convert the in_channel to a stream of characters.

   DO NOT MODIFY `in_chan` AFTER CALLING THIS! *)
val of_in_channel : in_channel -> char t

(** Creates a stream by repeatedly (lazily!) repeating a function *)
val of_iter : (unit -> 'b option) -> 'b t

val of_list : 'a list -> 'a t

