val wait_to_status : 'r Eio.Process.t -> int

val compose :
  sw:Eio.Switch.t ->
  mgr:[> ([> `Generic ] as 'a) Eio.Process.mgr_ty ] Eio.Std.r ->
  env:string array ->
  (string * string list) list ->
  'a Eio.Process.ty Eio.Std.r

val compose_stdin :
  sw:Eio.Switch.t ->
  mgr:[> ([> `Generic ] as 'a) Eio.Process.mgr_ty ] Eio.Std.r ->
  env:string array ->
  (string * string list) list ->
  [ `Flow | `R ] Eio.Flow.source ->
  'a Eio.Process.ty Eio.Std.r

val compose_stdout :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  env:string array ->
  (string * string list) list ->
  string * int

val compose_in_out :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  env:string array ->
  (string * string list) list ->
  [ `Flow | `R ] Eio.Flow.source ->
  string * int
