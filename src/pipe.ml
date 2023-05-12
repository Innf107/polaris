open Eio_unix

module Low_level = Eio_posix.Low_level
module Process = Low_level.Process
module Fd = Low_level.Fd

let wait_to_status process =
  match Eio.Promise.await (Process.exit_status process) with
  | Unix.WEXITED s | Unix.WSTOPPED s | Unix.WSIGNALED s ->
     s

let make_execve program arguments env =
  let program_path =
    let path_var = Unix.getenv "PATH" in
    let dir_path = 
      List.find 
        begin fun path -> 
          try
            Array.mem program (Low_level.readdir path)
          with
          | _ -> false
        end
        (String.split_on_char ':' path_var)
    in
    dir_path ^ "/" ^ program
  in
  Process.Fork_action.execve program_path ~argv:(Array.of_list (program_path :: arguments)) ~env

let make_pipe_fd ~sw pipe_end = 
  let unix_fd = pipe_end#unix_fd `Peek in
  Unix.set_nonblock unix_fd;
  Low_level.Fd.of_unix ~sw ~blocking:false ~close_unix:true unix_fd

let rec compose_pipe ~sw ~env (stdin : Fd.t option) (stdout : Fd.t option) = function
    | [] -> Util.panic __LOC__ "compose_pipe: called on an empty program list"
    | [(program, arguments)] ->
      let inherited_fds = 
        List.filter_map (fun (x, y) -> Option.map (fun y -> (x, y, `Preserve_blocking)) y)
          [(0, stdin); (1, stdout)]
      in
      let proc = Process.spawn ~sw
        [ Process.Fork_action.inherit_fds inherited_fds
        ; make_execve program arguments env 
        ] in
      proc
    | (program, arguments) :: rest ->
      let (pipe_read, pipe_write) = Low_level.pipe ~sw in
      let inherited_fds = match stdin with
      | None ->       [ (1, pipe_write, `Preserve_blocking) ]
      | Some stdin -> [ (0, stdin, `Preserve_blocking); (1, pipe_write, `Preserve_blocking) ]
      in
      let _ = Process.spawn ~sw
        [ Process.Fork_action.inherit_fds inherited_fds
        ; make_execve program arguments env
        ] in
      compose_pipe ~sw ~env (Some pipe_read) stdout rest

let compose ~sw ~env = compose_pipe ~sw ~env None None

let compose_in ~sw ~env progs =
    let (pipe_read, pipe_write) = Eio_unix.pipe sw in

    let fd = Option.get (pipe_write#probe Low_level.Fd.FD) in

    let process = compose_pipe ~sw ~env None (Some (fd)) progs in
    Low_level.Fd.close fd;

    (pipe_read :> < Eio.Flow.source; Eio.Flow.close >), process

let compose_out ~sw ~env progs =
  let pipe_read, pipe_write = Eio_unix.pipe sw in

  let process = compose_pipe ~sw ~env (Some (make_pipe_fd ~sw pipe_read)) None progs in

  (pipe_write :> < Eio.Flow.sink; Eio.Flow.close >), process



let compose_out_with ~sw ~env progs f =
  let sink, process = compose_out ~sw ~env progs in
  f (sink :> Eio.Flow.sink);
  Eio.Flow.close sink;
  process

let compose_in_out ~sw ~env progs f =
  let in_pipe_read, in_pipe_write = Eio_unix.pipe sw in
  let out_pipe_read, out_pipe_write = Eio_unix.pipe sw in
  
  let process = 
    compose_pipe 
      ~sw 
      ~env 
      (Some (make_pipe_fd ~sw in_pipe_read)) 
      (Some (make_pipe_fd ~sw out_pipe_write)) 
      progs 
  in

  Eio.Fiber.fork ~sw begin fun () ->
    f (in_pipe_write :> Eio.Flow.sink);
    Eio.Flow.close in_pipe_write
  end;
  (out_pipe_read :> < Eio.Flow.source; Eio.Flow.close >), process

