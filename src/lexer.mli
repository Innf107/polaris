type lex_error =
  | InvalidOperator of Syntax.loc * string
  | InvalidChar of Syntax.loc * char
  | UnterminatedString

exception LexError of lex_error

type lex_state

val new_lex_state : unit -> lex_state
val token : lex_state -> Lexing.lexbuf -> Parser.token
