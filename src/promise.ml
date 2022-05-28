open Thread
open Util

module Promise = struct
  type 'a result = Finished of 'a
                 | Failed of exn
                 | Pending
  
  type 'a t = {
    thread : Thread.t;
    result_ref : 'a result ref; 
  }

  let create f =
    let result = ref Pending in
    let t = Thread.create begin fun result ->
      try
        result := Finished (f ())
      with
      | ex -> result := Failed ex
    end result in
    {
      thread = t;
      result_ref = result
    }
  
  let peek (p : 'a t) = !(p.result_ref)

  let await (p : 'a t) =
    Thread.join p.thread;
    match !(p.result_ref) with
    | Finished value -> value
    | Failed ex -> raise ex
    | Pending -> raise (Panic "Pending Promise after thread join during await")
end
