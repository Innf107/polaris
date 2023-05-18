open Eio_unix

let wait_to_status process =
  match Eio.Process.await process with
  | `Exited s | `Signaled s -> s

  
let rec compose_pipe ~mgr ~sw ~env (stdin : #Eio.Flow.source option) (stdout : #Eio.Flow.sink option) = function
    | [] -> Util.panic __LOC__ "compose_pipe: called on an empty program list"
    | [(program, arguments)] ->
      Eio.Process.spawn ~sw mgr ?stdin ?stdout ~env (program :: arguments)
    | (program, arguments) :: rest ->
      let (pipe_read, pipe_write) = Eio.Process.pipe ~sw mgr in

      let pipe_read : #Eio.Flow.source = pipe_read in

      let _ = Eio.Process.spawn ~sw mgr ?stdin ~stdout:pipe_write ~env (program :: arguments) in
      compose_pipe ~mgr ~sw ~env (Some (Obj.magic (pipe_read :> Eio.Flow.source))) stdout rest

let compose ~sw ~mgr ~env progs : Eio.Process.t = compose_pipe ~sw ~mgr ~env None None progs

let compose_stdin ~sw ~mgr ~env progs source =
  compose_pipe ~sw ~mgr ~env (Some source) None progs


let compose_stdout ~sw ~mgr ~env progs sink =
  compose_pipe ~sw ~mgr ~env None (Some sink) progs



let compose_in_out ~sw ~mgr ~env progs source sink =
  compose_pipe ~sw ~mgr ~env (Some source) (Some sink) progs
