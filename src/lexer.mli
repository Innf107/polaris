type lex_error =
  | InvalidOperator of Syntax.loc * string
  | InvalidChar of Syntax.loc * string
  | UnterminatedString
  | InvalidStringEscape of Syntax.loc * string

exception LexError of lex_error

type lex_state

val current_positions : lex_state -> Lexing.position * Lexing.position

val new_lex_state : Sedlexing.lexbuf -> lex_state
val token : lex_state -> Parser.token
