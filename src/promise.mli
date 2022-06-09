
type 'a result = Finished of 'a
               | Failed of exn
               | Pending

type 'a t

val create : (unit -> 'a) -> 'a t 

val peek : 'a t -> 'a result

val await : 'a t -> 'a

val await_remaining : unit -> unit
