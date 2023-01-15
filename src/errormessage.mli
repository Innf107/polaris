
type color = Red | Purple | Custom of string

val make_coloring : enable_color:bool -> color -> string -> string


type text_style = {
   color : color -> string -> string;
   bold : string -> string;
}

val make_text_style : enable_color:bool -> text_style

module Style : sig
   type t

   val plain : t

   val with_color : color -> t -> t
   val with_underline : char -> color -> t -> t
   val with_bold : t -> t
   val with_raw_transform : (string -> string) -> t -> t

   val disable_color : t -> t

   val is_color_enabled : t -> bool
end


(* Extracts a fragment of code from the source code of a file
   based on a given location. This takes a continuation that styles the exact fragment
*)
val extract_source_fragment : Loc.t 
                           -> In_channel.t 
                           -> Style.t
                           -> (string * string * string)



