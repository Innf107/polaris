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

  let get_loc _ = {
    file = "TODO"
  ; start_line = 0
  ; start_col = 0
  ; end_line = 0
  ; end_col = 0
  }
end

module AthenaInst = Athena.Make(Token)
open AthenaInst

let (let*) = AthenaInst.bind
let (and*) = AthenaInst.prod


let pretty_error = function
  | RemainingTokens toks -> "Unexpected remaining tokens. Expected end of file"
  | UnexpectedEOF -> "Unexpected end of file"
  | ParseError msg -> msg
  | ParseErrorOn (msg, tok) -> "Unexpected '" ^ Token.pretty tok ^ "': " ^ msg
  | UnexpectedToken tok -> "Unexpected '" ^ Token.pretty tok ^ "'"

let ident = token_of begin function
  | Token.IDENT id -> Some(id)
  | _ -> None
  end

let string = token_of begin function
  | Token.STRING str -> Some(str)
  | _ -> None
  end

let int = token_of begin function
| Token.INT i -> Some(i)
| _ -> None
end

let float = token_of begin function
| Token.FLOAT i -> Some(i)
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
  <*> optional header_options                               (* options *)

let rec expr stream = begin
  let op = 
      (let* t = token OR in pure (fun e1 e2 -> Or(Token.get_loc t, e1, e2)))
  <|> (let* t = token AND in pure (fun e1 e2 -> And(Token.get_loc t, e1, e2))) 
  in
  chainl1 expr1 op
end stream

and expr1 stream = begin
  let op = 
      (let* t = token BANGEQUALS in pure (fun e1 e2 -> NotEquals(Token.get_loc t, e1, e2)))
  <|> (let* t = token DOUBLEEQUALS in pure (fun e1 e2 -> Equals(Token.get_loc t, e1, e2))) 
  <|> (let* t = token LT in pure (fun e1 e2 -> LT(Token.get_loc t, e1, e2))) 
  <|> (let* t = token GT in pure (fun e1 e2 -> GT(Token.get_loc t, e1, e2)))
  <|> (let* t = token LE in pure (fun e1 e2 -> LE(Token.get_loc t, e1, e2))) 
  <|> (let* t = token GE in pure (fun e1 e2 -> GE(Token.get_loc t, e1, e2))) 
  in
  chainl1 expr2 op
end stream
  
and expr2 stream = begin
  let op = 
      (let* t = token PLUS in pure (fun e1 e2 -> Add(Token.get_loc t, e1, e2)))
  <|> (let* t = token MINUS in pure (fun e1 e2 -> Sub(Token.get_loc t, e1, e2))) 
  <|> (let* t = token TILDE in pure (fun e1 e2 -> Concat(Token.get_loc t, e1, e2))) 
  in
  chainl1 expr3 op
end stream

and expr3 stream = begin
  let op = 
      (let* t = token STAR in pure (fun e1 e2 -> Mul(Token.get_loc t, e1, e2)))
  <|> (let* t = token SLASH in pure (fun e1 e2 -> Div(Token.get_loc t, e1, e2))) 
  in
  chainl1 expr_leaf op
end stream

and expr_leaf stream = begin
      ((fun loc x -> StringLit (loc, x)) <$$> string)                                                                       (* "str" *)
  <|> ((fun loc x -> NumLit (loc, float_of_int x)) <$$> int)                                                                (* n *)
  <|> ((fun loc x -> NumLit (loc, x)) <$$> float)                                                                           (* f *)
  <|> ((fun loc _ -> UnitLit loc) <$$> token LPAREN *> token RPAREN)                                                        (* () *)
  <|> ((fun loc _ -> BoolLit (loc, true)) <$$> token TRUE)                                                                  (* true *)
  <|> ((fun loc _ -> BoolLit (loc, false)) <$$> token FALSE)                                                                (* false *)
  <|> ((fun loc _ -> NullLit loc) <$$> token NULL)                                                                          (* null *)
  <|> ((fun loc x -> Var(loc, x)) <$$> ident)                                                                               (* x *)
  <|> ((fun loc es -> ListLit (loc, es)) <$$> (token LBRACKET *> sep_by_trailing (token COMMA) expr_leaf <* token RBRACKET))     (* [ e, .., e ] *)
  (* TODO: No list comprehensions for now *)

  <|> ((fun loc _ -> todo __POS__)                                                                                          (* { x : e, .., x : e } *)
    <$$> token HASHLBRACE 
      *> sep_by_trailing (token COMMA) ((fun x y -> (x, y)) <$> ident <* token COLON <*> expr_leaf)
     <*  token RBRACE)
  <|> (token NOT *> ((fun l e -> Not(l, e)) <$$> expr_leaf))
  end stream

let entry = (fun h exprs -> (h, exprs) )
  <$> many (token SEMI)
   *> header
  <*  many (token SEMI)
  <*> sep_by_trailing (token SEMI) expr


let main : Token.t Athena.Stream.t -> (header * expr list, AthenaInst.parse_error) result = 
  fun stream ->
    parse entry stream

