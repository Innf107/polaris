open Util
open Lexing

type lex_error =
  | InvalidOperator of Syntax.loc * string
  | InvalidChar of Syntax.loc * string
  | UnterminatedString
  | InvalidStringEscape of Syntax.loc * string

exception LexError of lex_error

(* Just after opening a new block (e.g. after '{'), the latest indentation_state
   is initially set to 'Opening'. The first non-whitespace character in this block
   sets the indentation_state, setting it to 'Found <indentation>'
*)
type indentation_state =
  | Opening
  | Found of int
  | OpeningInterpolation of [ `Single | `Double ] * position
  | FoundInterpolation of [ `Single | `Double ] * position * int

type lex_state = {
  mutable indentation_level : indentation_state list;
  mutable next_starting_state :
    [ `LexString of [ `Single | `Double ] * position ] option;
  lexbuf : Sedlexing.lexbuf;
  deferred_tokens : Parser.token Queue.t;
  mutable start_position_override : Lexing.position option;
}

let new_lex_state lexbuf =
  {
    indentation_level = [ Found 1 ];
    lexbuf;
    next_starting_state = None;
    deferred_tokens = Queue.create ();
    start_position_override = None;
  }

let current_positions lex_state =
  let start_loc, end_loc = Sedlexing.lexing_positions lex_state.lexbuf in
  match lex_state.start_position_override with
  | Some override -> (override, end_loc)
  | None -> (start_loc, end_loc)

let open_block state =
  state.indentation_level <- Opening :: state.indentation_level

let open_interpolation_block kind start_position state =
  state.indentation_level <-
    OpeningInterpolation (kind, start_position) :: state.indentation_level

let get_loc lexbuf =
  let start_pos, end_pos = Sedlexing.lexing_positions lexbuf in
  Loc.from_pos start_pos end_pos

let ident_token =
  let open Parser in
  function
  | "let" -> LET
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "true" -> TRUE
  | "false" -> FALSE
  | "async" -> ASYNC
  | "await" -> AWAIT
  | "match" -> MATCH
  | "usage" -> USAGE
  | "description" -> DESCRIPTION
  | "options" -> OPTIONS
  | "as" -> AS
  | "not" -> NOT
  | "with" -> WITH
  | "extend" -> EXTEND
  | "module" -> MODULE
  | "import" -> IMPORT
  | "export" -> EXPORT
  | "forall" -> FORALL
  | "data" -> DATA
  | "type" -> TYPE
  | "ref" -> REF
  | "exception" -> EXCEPTION
  | "try" -> TRY
  | "raise" -> RAISE
  | str -> IDENT str

(* TODO: this should probably be something more intelligent *)
let program_call_character =
  [%sedlex.regexp? Compl (white_space | '(' | ')' | '[' | ']' | '{' | '}')]

let program_call_start =
  [%sedlex.regexp?
    Compl
      ( white_space | '(' | ')' | '[' | ']' | '{' | '}' | '.' | ',' | ':' | ';'
      | '<' | '>' | '+' | '*' | '&' | '|' | '~' | '-' | '-' | '=' | '$' | '\\'
      | '#' | '!' )]

(* TODO: allow more than this somehow. maybe $"env" syntax?*)
let env_var_character = [%sedlex.regexp? alphabetic | '0' .. '9' | '_' | '-']

let lexeme_skip start_offset end_offset lexbuf =
  Sedlexing.Utf8.sub_lexeme lexbuf start_offset
    (Sedlexing.lexeme_length lexbuf - end_offset - start_offset)

let rec lex lex_state =
  let lexbuf = lex_state.lexbuf in
  let lexeme () = Sedlexing.Utf8.lexeme lexbuf in

  match%sedlex lexbuf with
  | '#', Star (Compl ('\n' | eof)) -> lex lex_state
  | Plus (Intersect (white_space, Compl '\n')) ->
      Sedlexing.start lexbuf;
      lex lex_state
  | '\n' -> leading_whitespace lex_state
  | uppercase, Star (alphabetic | '0' .. '9' | '_') ->
      Parser.CONSTRUCTOR (lexeme ())
  | (lowercase | '_'), Star (alphabetic | '0' .. '9' | '_') ->
      ident_token (lexeme ())
  | '"' ->
      let start_pos, _end_pos = Sedlexing.lexing_positions lexbuf in
      lex_string `Double (Cowbuffer.create 16) true start_pos lex_state
  | '\'' ->
      let start_pos, _end_pos = Sedlexing.lexing_positions lexbuf in
      lex_string `Single (Cowbuffer.create 16) true start_pos lex_state
  | '`' -> Parser.BACKTICK
  | '!' -> Parser.BANG
  | "!=" -> Parser.BANGEQUALS
  (* We special case !./ and !../ since those are useful uses of ! followed by a dot.
     We disallow all other operator characters in the first character of an ! expression *)
  | "!./", Plus program_call_character ->
      Parser.PROGCALL (lexeme_skip 1 0 lexbuf)
  | "!../", Plus program_call_character ->
      Parser.PROGCALL (lexeme_skip 1 0 lexbuf)
  | '!', program_call_start, Star program_call_character ->
      Parser.PROGCALL (lexeme_skip 1 0 lexbuf)
  | '$', Plus env_var_character -> Parser.ENVVAR (lexeme_skip 1 0 lexbuf)
  | "->" -> Parser.ARROW
  | "<-" -> Parser.LARROW
  | ',' -> Parser.COMMA
  | ';' -> Parser.SEMI
  | ':' -> Parser.COLON
  | "::" -> Parser.DOUBLECOLON
  | '+' -> Parser.PLUS
  | '*' -> Parser.STAR
  | '/' -> Parser.SLASH
  | "||" -> Parser.OR
  | "&&" -> Parser.AND
  | '.' -> Parser.DOT
  | ".." -> Parser.DDOT
  | '~' -> Parser.TILDE
  | '|' -> Parser.PIPE
  | '=' -> Parser.EQUALS
  | ":=" -> Parser.COLONEQUALS
  | "==" -> Parser.DOUBLEEQUALS
  | '<' -> Parser.LT
  | '>' -> Parser.GT
  | "<=" -> Parser.LE
  | ">=" -> Parser.GE
  | '\\' -> Parser.LAMBDA
  | '-' -> Parser.MINUS
  | Opt '-', Plus '0' .. '9' -> Parser.INT (int_of_string (lexeme ()))
  | Opt '-', Star '0' .. '9', '.', Plus '0' .. '9'
  | Opt '-', Plus '0' .. '9', '.', Star '0' .. '9' ->
      Parser.FLOAT (float_of_string (lexeme ()))
  (* Special case for 0.. which should be lexed as the start of a range like 0 .. 9
     rather than a float literal followed by a period (0. . 9)
     Since sedlex prioritizes the longest match, this will override the previous case if possible*)
  | Opt '-', Plus '0' .. '9', ".." ->
      Queue.add Parser.DDOT lex_state.deferred_tokens;
      Parser.INT (int_of_string (lexeme_skip 0 2 lexbuf))
  | '(' -> Parser.LPAREN
  | ')' -> Parser.RPAREN
  | '[' -> Parser.LBRACKET
  | ']' -> Parser.RBRACKET
  | '{' ->
      open_block lex_state;
      Parser.LBRACE
  | '}' -> begin
      match lex_state.indentation_level with
      | []
      | [ _ ] ->
          panic __LOC__ "Lexer.close_block: more blocks closed than opened"
      | ( OpeningInterpolation (kind, start_position)
        | FoundInterpolation (kind, start_position, _) )
        :: lvls ->
          lex_state.indentation_level <- lvls;
          lex_state.next_starting_state <-
            Some (`LexString (kind, start_position));
          Parser.INTERPOLATION_END
      | (Found _ | Opening) :: lvls ->
          lex_state.indentation_level <- lvls;
          Parser.RBRACE
    end
  | eof -> Parser.EOF
  | _ -> raise (LexError (InvalidChar (get_loc lexbuf, lexeme ())))

(* TODO: keep track of the starting position i guess? *)
and lex_string kind buffer is_initial start_position lex_state =
  let lexbuf = lex_state.lexbuf in
  let lexeme () = Sedlexing.Utf8.lexeme lexbuf in

  let end_string () =
    lex_state.start_position_override <- Some start_position;

    if is_initial then begin
      Parser.STRING (Cowbuffer.contents buffer)
    end
    else
      match Cowbuffer.length buffer with
      | 0 -> Parser.INTERP_STRING_END
      | _ ->
          Queue.add Parser.INTERP_STRING_END lex_state.deferred_tokens;
          Parser.STRING_COMPONENT (Cowbuffer.contents buffer)
  in

  match%sedlex lexbuf with
  | Plus (Compl ('"' | '\'' | '\\' | '$')) ->
      lex_string kind
        (Cowbuffer.add_string (lexeme ()) buffer)
        is_initial start_position lex_state
  | '"' -> begin
      match kind with
      | `Single ->
          lex_string kind
            (Cowbuffer.add_char '"' buffer)
            is_initial start_position lex_state
      | `Double -> end_string ()
    end
  | '\'' -> begin
      match kind with
      | `Single -> end_string ()
      | `Double ->
          lex_string kind
            (Cowbuffer.add_char '\'' buffer)
            is_initial start_position lex_state
    end
  (* sedlex prioritises the longest match so the second case will always take precedence if possible*)
  | '$' ->
      lex_string kind
        (Cowbuffer.add_char '$' buffer)
        is_initial start_position lex_state
  | "${" ->
      open_interpolation_block kind start_position lex_state;

      if is_initial then begin
        Queue.add Parser.INTERP_STRING_START lex_state.deferred_tokens
      end;

      begin
        match Cowbuffer.length buffer with
        | 0 -> Queue.add Parser.INTERPOLATION_START lex_state.deferred_tokens
        | _ ->
            Queue.add
              (Parser.STRING_COMPONENT (Cowbuffer.contents buffer))
              lex_state.deferred_tokens;
            Queue.add Parser.INTERPOLATION_START lex_state.deferred_tokens
      end;
      Queue.take lex_state.deferred_tokens
  | "\\a" ->
      lex_string kind
        (Cowbuffer.add_char '\x07' buffer)
        is_initial start_position lex_state
  | "\\b" ->
      lex_string kind
        (Cowbuffer.add_char '\x08' buffer)
        is_initial start_position lex_state
  | "\\e" ->
      lex_string kind
        (Cowbuffer.add_char '\x1b' buffer)
        is_initial start_position lex_state
  | "\\f" ->
      lex_string kind
        (Cowbuffer.add_char '\x0C' buffer)
        is_initial start_position lex_state
  | "\\n" ->
      lex_string kind
        (Cowbuffer.add_char '\n' buffer)
        is_initial start_position lex_state
  | "\\r" ->
      lex_string kind
        (Cowbuffer.add_char '\r' buffer)
        is_initial start_position lex_state
  | "\\t" ->
      lex_string kind
        (Cowbuffer.add_char '\t' buffer)
        is_initial start_position lex_state
  | "\\v" ->
      lex_string kind
        (Cowbuffer.add_char '\x0B' buffer)
        is_initial start_position lex_state
  | "\\\\" ->
      lex_string kind
        (Cowbuffer.add_char '\\' buffer)
        is_initial start_position lex_state
  | "\\'" ->
      lex_string kind
        (Cowbuffer.add_char '\'' buffer)
        is_initial start_position lex_state
  | "\\\"" ->
      lex_string kind
        (Cowbuffer.add_char '"' buffer)
        is_initial start_position lex_state
  | "\\$" ->
      lex_string kind
        (Cowbuffer.add_char '$' buffer)
        is_initial start_position lex_state
  | "\\x", Rep (('0' .. '9' | 'a' .. 'f' | 'A' .. 'F'), 1 .. 6) ->
      let escape =
        Sedlexing.Utf8.sub_lexeme lexbuf 2 (Sedlexing.lexeme_length lexbuf - 2)
      in
      let parse_hex str =
        let rec go i weight =
          if i < 0 then 0
          else
            let digit_value =
              match str.[i] with
              | '0' .. '9' as char -> int_of_char char - int_of_char '0'
              | 'a' .. 'f' as char -> 10 + int_of_char char - int_of_char 'a'
              | 'A' .. 'F' as char -> 10 + int_of_char char - int_of_char 'A'
              | char ->
                  panic __LOC__
                    ("invalid hex digit lexed: " ^ String.make 1 char)
            in
            (digit_value * weight) + go (i - 1) (weight * 16)
        in
        go (String.length str - 1) 1
      in
      let uchar =
        try Uchar.of_int (parse_hex escape) with
        | Invalid_argument _ ->
            raise_notrace
              (LexError (InvalidStringEscape (get_loc lexbuf, "x" ^ escape)))
      in
      let buffer = Cowbuffer.add_utf_8_uchar uchar buffer in
      lex_string kind buffer is_initial start_position lex_state
  | "\\", Compl (Chars "abefnrtv\\\'\"$x") ->
      raise (LexError (InvalidStringEscape (get_loc lexbuf, lexeme ())))
  | _ -> todo __LOC__

and leading_whitespace lex_state =
  let lexbuf = lex_state.lexbuf in
  match%sedlex lexbuf with
  | Plus white_space ->
      Sedlexing.start lexbuf;
      leading_whitespace lex_state
  | '#', Star (Compl ('\n' | eof)) -> leading_whitespace lex_state
  | eof -> Parser.EOF
  | _ ->
      let _ = Sedlexing.backtrack lexbuf in
      emit_semi_or_lex lex_state

and emit_semi_or_lex lex_state =
  let loc = get_loc lex_state.lexbuf in
  match lex_state.indentation_level with
  | [] -> panic __LOC__ "more blocks closed than opened"
  | Opening :: levels ->
      lex_state.indentation_level <- Found loc.start_col :: levels;
      lex lex_state
  | OpeningInterpolation (string_kind, start_location) :: levels ->
      lex_state.indentation_level <-
        FoundInterpolation (string_kind, start_location, loc.start_col)
        :: levels;
      lex lex_state
  | Found block_indentation :: levels
  | FoundInterpolation (_, _, block_indentation) :: levels ->
      if loc.start_col <= block_indentation then Parser.SEMI else lex lex_state

let token lex_state =
  lex_state.start_position_override <- None;
  match Queue.take_opt lex_state.deferred_tokens with
  | Some token -> token
  | None -> begin
      match lex_state.next_starting_state with
      | None -> lex lex_state
      | Some alternative_state -> begin
          lex_state.next_starting_state <- None;
          match alternative_state with
          | `LexString (kind, start_position) ->
              lex_string kind (Cowbuffer.create 16) false start_position
                lex_state
        end
    end
