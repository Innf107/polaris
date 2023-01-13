
(* Extracts a fragment of code from the source code of a file
   based on a given location. This takes a continuation that styles the exact fragment
*)
val extract_source_fragment : Loc.t -> In_channel.t -> (string -> string) -> (string * string * string) 



