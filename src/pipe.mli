val wait_to_status : Eio.Process.t -> int

val compose :
  sw:Eio.Switch.t ->
  mgr:Eio.Process.mgr ->
  env:string array ->
  (string * string list) list ->
  Eio.Process.t

val compose_stdin :
  sw:Eio.Switch.t ->
  mgr:Eio.Process.mgr ->
  env:string array ->
  (string * string list) list ->
  #Eio.Flow.source ->
  Eio.Process.t

val compose_stdout :
  sw:Eio.Switch.t ->
  mgr:Eio.Process.mgr ->
  env:string array ->
  (string * string list) list ->
  string * int

val compose_in_out :
  sw:Eio.Switch.t ->
  mgr:Eio.Process.mgr ->
  env:string array ->
  (string * string list) list ->
  #Eio.Flow.source ->
  string * int
