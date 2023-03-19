type t = {    
    file : string;
    start_line : int;
    start_col : int;
    end_line : int;
    end_col : int
}

val pretty : t -> string

(* Like pretty but only displays the starting position, which ensures that
   locs are clickable in editors like Visual Studio Code *)
val pretty_start : t -> string

val from_pos : Lexing.position -> Lexing.position -> t

val internal : t

val is_internal : t -> bool

val merge : t -> t -> t

val concat : t list -> t

val concat_split : ('a * t) list -> 'a list * t