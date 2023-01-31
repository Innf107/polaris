
val send_notification : string -> Yojson.Basic.t -> unit

val run : in_channel 
        -> out_channel
        -> handler:(request_method:string -> Yojson.Basic.t -> Yojson.Basic.t) 
        -> notification_handler:(notification_method:string -> Yojson.Basic.t -> unit)
        -> unit

val unsupported_method : unit -> 'a

