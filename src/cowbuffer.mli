type t
(** A copy-on write wrapper for Buffer.t. Any linear modifications of this buffer will happen in-place
    and any non-linear modifications to a buffer that has been modified before will create a copy,
    thereby maintaining the pure interface. *)

val as_cow_unchecked : ?size_hint:(int -> int) -> (Buffer.t -> unit) -> t -> t
(** Transform a mutating buffer transformation into a pure copy-on-write operation.
    The optional parameter ?size_hint can be used to influence the initial capacity of the buffer in case
    of a copy.

    WARNING: For this to be safe, the passed transformation must not shrink the buffer or modify its contents.
    This property is not checked. *)

val create : int -> t
val length : t -> int
val contents : t -> string
val add_char : char -> t -> t
val add_string : string -> t -> t
val add_substring : string -> int -> int -> t -> t
val add_utf_8_uchar : Uchar.t -> t -> t
val nth : t -> int -> char
