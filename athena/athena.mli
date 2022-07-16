type loc = Loc.t

module Stream = Stream
module Loc = Loc

include module type of Loc

module Make(Token : sig
    type t

    val to_string : t -> string

    val equal : t -> t -> bool
end) : sig

    type parse_error = RemainingTokens of (Token.t * loc) Stream.t
                    | UnexpectedEOF
                    | ParseError of string
                    | ParseErrorOn of string * Token.t * loc
                    | UnexpectedToken of Token.t * loc

    type 'a parser_impl
    type parser_arg
    type 'a parser = parser_arg -> 'a parser_impl

    val parse : 'a parser -> (Token.t * loc) Stream.t -> ('a, parse_error) result

    val map : ('a -> 'b) -> 'a parser -> 'b parser

    val pure : 'a -> 'a parser

    val (<*>) : ('a -> 'b) parser -> 'a parser -> 'b parser
    val ( *>) : 'a parser -> 'b parser -> 'b parser
    val (<* ) : 'a parser -> 'b parser -> 'a parser
    val (<$>) : ('a -> 'b) -> 'a parser -> 'b parser

    val prod : 'a parser -> 'b parser -> ('a * 'b) parser

    val bind  : 'a parser -> ('a -> 'b parser) -> 'b parser
    val (>>=) : 'a parser -> ('a -> 'b parser) -> 'b parser
    val (let*) : 'a parser -> ('a -> 'b parser) -> 'b parser

    val (<|>) : 'a parser -> 'a parser -> 'a parser

    val fail : string -> 'a parser
    val fail_error : parse_error -> 'a parser

    val (<?>) : 'a parser -> string -> 'a parser
    val (<??>) : string -> 'a parser -> 'a parser

    val any : (Token.t * loc) parser

    val satisfy_loc : (loc -> Token.t -> bool) -> (Token.t * loc) parser
    val satisfy : (Token.t -> bool) -> loc parser

    val token : Token.t -> loc parser

    val token_of : (loc -> Token.t -> 'a option) -> 'a parser

    val one_of : Token.t list -> (Token.t * loc) parser

    (** matches zero or more *)
    val many : 'a parser -> 'a list parser

    (** matches one or more *)
    val some : 'a parser -> 'a list parser

    val sep_by : 'a parser -> 'b parser -> 'b list parser
    val sep_by1 : 'a parser -> 'b parser -> 'b list parser
    val sep_by_trailing : 'a parser -> 'b parser -> 'b list parser
    val sep_by_trailing1 : 'a parser -> 'b parser -> 'b list parser

    val optional : 'a parser -> 'a option parser

    val chainl : 'a parser -> ('a -> 'a -> 'a) parser -> 'a -> 'a parser

    val chainl1 : 'a parser -> ('a -> 'a -> 'a) parser -> 'a parser

    val left_assoc : 'a parser -> ('a -> 'a) parser -> 'a parser
end