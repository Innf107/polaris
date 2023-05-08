open Unix

let wait_to_status pid = 
  let _, status = waitpid [] pid in
  match status with
  (* TODO: This is probably not how WSIGNALED and WSTOPPED should be treated *)
  | WEXITED status | WSIGNALED status | WSTOPPED status -> status

let rec compose_destructive (env : string array option) = function
| [] -> failwith "unable to compose empty arguments"
| [(prog, prog_args)] ->
  begin match env with
  | None -> execvp prog (Array.of_list (prog::prog_args))
  | Some env -> execvpe prog (Array.of_list (prog::prog_args)) env
  end
| ((prog, prog_args) :: progs) -> 
  let (pipe_read, pipe_write) = pipe () in
  match fork () with
  | 0 ->
    dup2 pipe_write stdout;
    close pipe_read;
    begin match env with
    | None -> execvp prog (Array.of_list (prog :: prog_args))
    | Some env -> execvpe prog (Array.of_list (prog :: prog_args)) env
    end
  | _ ->
    dup2 pipe_read stdin;
    close pipe_write;
    compose_destructive env progs

let compose (env : string array option) progs =
  match fork () with
  | 0 ->
    compose_destructive env progs
  | pid ->
    pid


let compose_in (env : string array option) progs : in_channel * int =
  let pipe_read, pipe_write = pipe () in
  match fork () with
  | 0 ->
    dup2 pipe_write stdout;
    close pipe_read;
    compose_destructive env progs
  | pid ->
    close pipe_write;
    in_channel_of_descr pipe_read, pid

let compose_out (env : string array option) progs : out_channel * int =
  let pipe_read, pipe_write = pipe () in
  match fork () with
  | 0 ->
    dup2 pipe_read stdin;
    close pipe_write;
    compose_destructive env progs
  | pid ->
    close pipe_read;
    out_channel_of_descr pipe_write, pid

let compose_out_with (env : string array option) progs f : int =
  let out_chan, pid = compose_out env progs in
  f out_chan;
  Out_channel.close out_chan;
  pid

let compose_in_out (env : string array option) progs f =
  let in_pipe_read, in_pipe_write = pipe () in
  let out_pipe_read, out_pipe_write = pipe () in
  match fork () with
  | 0 ->
    dup2 out_pipe_read stdin;
    dup2 in_pipe_write stdout;
    close out_pipe_write;
    close in_pipe_read;
    compose_destructive env progs
  | pid ->
    close out_pipe_read;
    close in_pipe_write;
    let _ = Thread.create (fun () -> 
      let out_chan = out_channel_of_descr out_pipe_write in
      f out_chan;
      Out_channel.close out_chan;
      ) () in
    in_channel_of_descr in_pipe_read, pid
