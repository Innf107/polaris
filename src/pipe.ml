open Eio_unix

let wait_to_status process =
  match Eio.Process.await process with
  | `Exited s
  | `Signaled s ->
      s

let program_or_local program =
  if String.contains program '/' then "./" ^ program else program

let rec compose_pipe :
    mgr:_ Eio.Process.mgr ->
    sw:Eio.Switch.t ->
    env:string array ->
    [> `Flow | `R] Eio.Flow.source option ->
    [> `Flow | `W] Eio.Flow.sink option ->
    (string * string list) list ->
    _ Eio.Process.t =
 fun ~mgr ~sw ~env stdin stdout -> function
  | [] -> Util.panic __LOC__ "compose_pipe: called on an empty program list"
  | [ (program, arguments) ] ->
      Eio.Process.spawn ~sw mgr ?stdin ?stdout ~env
        (program_or_local program :: arguments)
  | (program, arguments) :: rest ->
      let pipe_read, pipe_write = Eio.Process.pipe ~sw mgr in

      let unclosable_pipe_read = (pipe_read :> [`Flow | `R] Eio.Flow.source) in

      let _ =
        Eio.Process.spawn ~sw mgr ?stdin ~stdout:pipe_write ~env
          (program_or_local program :: arguments)
      in
      compose_pipe ~mgr ~sw ~env (Some unclosable_pipe_read) stdout rest

let compose :
    sw:Eio.Switch.t ->
    mgr:[> [> `Generic ] Eio.Process.mgr_ty ] Eio.Std.r ->
    env:string array ->
    (string * string list) list ->
    [> `Generic ] Eio.Process.ty Eio.Std.r =
 fun ~sw ~mgr ~env progs -> compose_pipe ~sw ~mgr ~env None None progs

let compose_stdin :
    sw:Eio.Switch.t ->
    mgr:[> ([> `Generic ] as 'a) Eio.Process.mgr_ty ] Eio.Std.r ->
    env:string array ->
    (string * string list) list ->
    [ `Flow | `R ] Eio.Flow.source ->
    'a Eio.Process.ty Eio.Std.r =
 fun ~sw ~mgr ~env progs source ->
  compose_pipe ~sw ~mgr ~env (Some source) None progs

let compose_stdout :
    sw:Eio.Switch.t ->
    mgr:_ Eio.Process.mgr ->
    env:string array ->
    (string * string list) list ->
    string * int =
 fun ~sw ~mgr ~env progs ->
  let pipe_read, pipe_write = Eio.Process.pipe ~sw mgr in

  let pipe_write : _ Eio.Flow.sink = pipe_write in

  let process = compose_pipe ~sw ~mgr ~env None (Some pipe_write) progs in
  Eio.Flow.close pipe_write;
  let result =
    Eio.Buf_read.parse_exn Eio.Buf_read.take_all ~max_size:max_int pipe_read
  in
  Eio.Flow.close pipe_read;
  let status = wait_to_status process in
  (result, status)

let compose_in_out ~sw ~mgr ~env progs source =
  let pipe_read, pipe_write = Eio.Process.pipe ~sw mgr in

  let process =
    compose_pipe ~sw ~mgr ~env (Some source) (Some pipe_write) progs
  in
  Eio.Flow.close pipe_write;
  let result =
    Eio.Buf_read.parse_exn Eio.Buf_read.take_all ~max_size:max_int pipe_read
  in
  Eio.Flow.close pipe_read;
  let status = wait_to_status process in
  (result, status)
