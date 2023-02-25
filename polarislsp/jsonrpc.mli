
val send_notification : string -> Yojson.Safe.t -> unit

val run : in_channel 
        -> out_channel
        -> handler:(request_method:string -> Yojson.Safe.t -> Yojson.Safe.t) 
        -> notification_handler:(notification_method:string -> Yojson.Safe.t -> unit)
        -> unit

val unsupported_method : unit -> 'a
val parse_error : string -> 'a

val position : (int * int) Jsonutil.json_parser