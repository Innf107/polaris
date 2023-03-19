
type text_style = {
  custom : color:string -> string -> string;

  bold : string -> string;

  identifier : string -> string;
  number : int -> string;
  ty : string -> string;
  ty_secondary : string -> string;

  emphasis : string -> string;

  error : string -> string;
  warning : string -> string;
}

val make_text_style : enable_color:bool -> text_style


(* Extracts a fragment of code from the source code of a file
   based on a given location. This takes a continuation that styles the exact fragment
*)
val extract_source_fragment : Loc.t 
                           -> In_channel.t 
                           -> text_style
                           -> [`Error | `Warning]
                           -> (string * string * string)



