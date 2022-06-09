open Thread
open Util

type 'a result = Finished of 'a
                | Failed of exn
                | Pending

type 'a t = {
  thread : Thread.t;
  result_ref : 'a result ref; 
}

module ThreadMap = Map.Make(Int)

let remaining : Thread.t ThreadMap.t ref = 
  ref ThreadMap.empty

let create f =
  let result = ref Pending in
  let t = Thread.create begin fun result ->
    try
      result := Finished (f ());
      remaining := ThreadMap.remove (Thread.id (Thread.self ())) !remaining
    with
    | ex -> 
      result := Failed ex;
      remaining := ThreadMap.remove (Thread.id (Thread.self ())) !remaining
  end result in
  remaining := ThreadMap.add (Thread.id t) t !remaining;
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

let await_remaining () =
  ThreadMap.iter (fun _ t -> Thread.join t) !remaining
