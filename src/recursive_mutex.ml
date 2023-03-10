
type t = {
  underlying : Mutex.t;
  current_thread_id : int option ref;
  uses : int ref
}

let create () = {
  underlying = Mutex.create ();
  current_thread_id = ref None;
  uses = ref 0;
}

let lock mutex =
  let current_thread = Thread.id (Thread.self ()) in
(* Let's hope this is actually thread safe *)
  match !(mutex.current_thread_id) with
  | Some thread_id when thread_id = current_thread ->
    incr mutex.uses
  | _ ->
    Mutex.lock mutex.underlying;
    mutex.current_thread_id := Some current_thread;
    mutex.uses := 1

let unlock mutex =
  assert (!(mutex.current_thread_id) = Some (Thread.id (Thread.self ())));
  decr mutex.uses;
  if !(mutex.uses) = 0 then begin
    mutex.current_thread_id := None;
    Mutex.unlock mutex.underlying
  end


