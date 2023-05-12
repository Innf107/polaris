
module Process = Eio_posix.Low_level.Process

val wait_to_status : Process.t -> int

val compose : sw:Eio.Switch.t -> env:string array -> (string * string list) list -> Process.t

val compose_in : sw:Eio.Switch.t 
              -> env:string array 
              -> (string * string list) list 
              -> < Eio.Flow.source; Eio.Flow.close > * Process.t

val compose_out : sw:Eio.Switch.t 
               -> env:string array 
               -> (string * string list) list 
               -> < Eio.Flow.sink; Eio.Flow.close > * Process.t

val compose_out_with : sw:Eio.Switch.t 
                    -> env:string array 
                    -> (string * string list) list 
                    -> (Eio.Flow.sink -> unit) 
                    -> Process.t

val compose_in_out : sw:Eio.Switch.t 
                  -> env:string array 
                  -> (string * string list) list 
                  -> (Eio.Flow.sink -> unit) 
                  -> < Eio.Flow.source; Eio.Flow.close > * Process.t
