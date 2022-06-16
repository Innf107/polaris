open Athena
open Syntax.Parsed
open Util

module Token = struct
  type t = 
  | IDENT of string
  | STRING of string
  | INT of int 
  | FLOAT of float
  | LET
  | IN
  | TRUE 
  | FALSE
  | NULL
  | LAMBDA 
  | ARROW
  | LARROW
  | DOT
  | COMMA 
  | SEMI
  | COLON
  | LPAREN 
  | RPAREN 
  | HASHLBRACE 
  | LBRACE 
  | RBRACE 
  | LBRACKET 
  | RBRACKET
  | EQUALS 
  | COLONEQUALS
  | BANGEQUALS 
  | DOUBLEEQUALS
  | LT
  | GT
  | LE
  | GE
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | TILDE
  | DDOT
  | OR 
  | AND 
  | NOT
  | BANG of string
  | PIPE
  | IF 
  | THEN 
  | ELSE
  | ASYNC 
  | AWAIT
  | MATCH
  | EOF
  | USAGE
  | DESCRIPTION
  | OPTIONS
  | AS

  let pretty = function
  | IDENT i -> "IDENT(" ^ i ^ ")"
  | STRING str -> "STRING(" ^ str ^ ")"
  | INT i -> "INT(" ^ Int.to_string i ^ ")" 
  | FLOAT f -> "FLOAT"
  | LET -> "LET"
  | IN -> "IN"
  | TRUE -> "TRUE" 
  | FALSE -> "FALSE"
  | NULL -> "NULL"
  | LAMBDA -> "LAMBDA" 
  | ARROW -> "ARROW"
  | LARROW -> "LARROW"
  | DOT -> "DOT"
  | COMMA -> "COMMA" 
  | SEMI  -> "SEMI"
  | COLON -> "COLON"
  | LPAREN -> "LPAREN" 
  | RPAREN -> "RPAREN" 
  | HASHLBRACE -> "HASHLBRACE" 
  | LBRACE -> "LBRACE" 
  | RBRACE -> "RBRACE" 
  | LBRACKET -> "LBRACKET" 
  | RBRACKET -> "RBRACKET"
  | EQUALS -> "EQUALS" 
  | COLONEQUALS -> "COLONEQUALS"
  | BANGEQUALS -> "BANGEQUALS" 
  | DOUBLEEQUALS -> "DOUBLEEQUALS"
  | LT -> "LT"
  | GT -> "GT"
  | LE -> "LE"
  | GE -> "GE"
  | PLUS -> "PLUS" 
  | MINUS -> "MINUS"
  | STAR -> "STAR"
  | SLASH -> "SLASH"
  | TILDE -> "TILDE"
  | DDOT ->  "DDOT"
  | OR -> "OR" 
  | AND -> "AND" 
  | NOT -> "NOT"
  | BANG str -> "BANG(" ^ str ^ ")"
  | PIPE -> "PIPE"
  | IF -> "IF" 
  | THEN -> "THEN" 
  | ELSE -> "ELSE"
  | ASYNC -> "ASYNC" 
  | AWAIT -> "AWAIT"
  | MATCH -> "MATCH"
  | EOF -> "EOF"
  | USAGE -> "USAGE"
  | DESCRIPTION -> "DESCRIPTION"
  | OPTIONS -> "OPTIONS"
  | AS -> "AS"

  let to_string = pretty

  let equal : t -> t -> bool = (=)
end

module AthenaInst = Athena.Make(Token)
open AthenaInst

let pretty_error = function
  | RemainingTokens toks -> "Unexpected remaining tokens. Expected end of file"
  | UnexpectedEOF -> "Unexpected end of file"
  | ParseError msg -> msg
  | ParseErrorOn (msg, tok) -> "Unexpected '" ^ Token.pretty tok ^ "': " ^ msg
  | UnexpectedToken tok -> "Unexpected '" ^ Token.pretty tok ^ "'"

let string = token_of begin function
  | Token.STRING str -> Some(str)
  | _ -> None
  end

let int = token_of begin function
| Token.INT i -> Some(i)
| _ -> None
end

(* TODO: named arguments *)
let argcount =
      (token LPAREN *> int <* token RPAREN)
  <|> (token LPAREN *> token STAR *> token RPAREN *> pure (-1))
  <|> pure 0

let option_def = (fun _ -> todo __POS__)
  <$> some string
  <*> optional argcount

let header_options = 
      token OPTIONS
   *> token LBRACE
   *> sep_by_trailing (token SEMI) option_def

let header = (fun u d opts -> { usage = u; description = d; options = Option.value ~default:[] opts })
  <$> optional (token USAGE *> token COLON *> string)       (* usage *)
  <*  many (token SEMI)
  <*> optional (token DESCRIPTION *> token COLON *> string) (* description *)
  <*  many (token SEMI)
  <*> optional header_options

let expr = todo __POS__

let entry = (fun h exprs -> (h, exprs) )
  <$> many (token SEMI)
   *> header
  <*  many (token SEMI)
  <*> sep_by_trailing (token SEMI) expr


let main : Token.t Athena.Stream.t -> (header * expr list, AthenaInst.parse_error) result = 
  fun stream ->
    parse entry stream

