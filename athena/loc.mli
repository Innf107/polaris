type t = {    
    file : string;
    start_line : int;
    start_col : int;
    end_line : int;
    end_col : int
}  

val pretty : t -> string

val from_pos : Lexing.position -> Lexing.position -> t

val internal : t

val merge : t -> t -> t

val concat : t list -> t

val concat_split : ('a * t) list -> 'a list * t
