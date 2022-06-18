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
  | RemainingTokens toks -> "Unexpected remaining tokens.\nNext 10 tokens: " ^ String.concat (" ") (List.map (Token.pretty) (Util.take 10 (Stream.collect toks)))
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

let bang = token_of begin function
| Token.BANG str -> Some(str)
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

let option_def = (fun flags arg_count flag_var default description -> {flags; flag_var; arg_count; default; description})
  <$> some string
  <*> argcount
  <*  token AS
  <*> ident
  <*> optional (token EQUALS *> string)
  <*> optional (token COLON *> string)

let header_options = 
      token OPTIONS
   *> token LBRACE
   *> sep_by_trailing (some (token SEMI)) option_def
  <*  token RBRACE 

let header = (fun u d opts -> { usage = u; description = d; options = Option.value ~default:[] opts })
  <$> optional (token USAGE *> token COLON *> string)       (* usage *)
  <*  many (token SEMI)
  <*> optional (token DESCRIPTION *> token COLON *> string) (* description *)
  <*  many (token SEMI)
  <*> optional header_options                               (* options *)

let rec pattern stream = begin
  chainl1 pattern1 (let* t = token PIPE in pure (fun p1 p2 -> OrPat(Token.get_loc t, p1, p2)))                          (* p | p *)
end stream

and pattern1 stream = begin
  chainl1 pattern_leaf (let* t = token COLON in pure (fun p1 p2 -> ConsPat(Token.get_loc t, p1, p2)))                   (* p : p *)
end stream

and pattern_leaf stream = begin
      ((fun loc ps -> ListPat(loc, ps)) <$$> token LBRACKET *> sep_by_trailing (token COMMA) pattern <* token RBRACKET) (* [p, .., p] *)
  <|> ((fun loc x -> VarPat(loc, x)) <$$> ident)                                                                        (* x *)
  <|> ((fun loc x -> NumPat(loc, float_of_int x)) <$$> int)                                                             (* n *)
  <|> ((fun loc x -> NumPat(loc, x)) <$$> float)                                                                        (* f *)
  <|> (token LPAREN *> pattern <* token RPAREN)                                                                         (* ( p ) *)
end stream

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
  chainl1 expr4 op
end stream

and expr4 stream = begin
  let ops =
      (let* t = token DOT in let* x = ident in pure (fun e -> MapLookup(Token.get_loc t, e, x)))
  <|> (let* t = token LBRACKET in let* e2 = expr in let* _ = token RBRACKET in pure (fun e -> DynLookup(Token.get_loc t, e, e2)))
  <|> (let* t = token LPAREN in let* es = sep_by_trailing (token COMMA) expr in let* _ = token RPAREN in pure (fun e -> App(Token.get_loc t, e, es)))
  in
      ((fun loc x e -> Assign(loc, x, e)) <$$> ident <* token COLONEQUALS <*> expr)                                         (* x := e *)
  <|> left_assoc expr_leaf ops
end stream

and expr_leaf stream = begin
      ((fun loc x -> StringLit (loc, x))           <$$> string)                                                             (* "str" *)
  <|> ((fun loc x -> NumLit (loc, float_of_int x)) <$$> int)                                                                (* n *)
  <|> ((fun loc x -> NumLit (loc, x))              <$$> float)                                                              (* f *)
  <|> ((fun loc _ -> UnitLit loc)                  <$$> token LPAREN *> token RPAREN)                                       (* () *)
  <|> ((fun loc _ -> BoolLit (loc, true))          <$$> token TRUE)                                                         (* true *)
  <|> ((fun loc _ -> BoolLit (loc, false))         <$$> token FALSE)                                                        (* false *)
  <|> ((fun loc _ -> NullLit loc)                  <$$> token NULL)                                                         (* null *)
  <|> ((fun loc x -> Var(loc, x))                  <$$> ident)                                                              (* x *)
  <|> ((fun loc es -> ListLit (loc, es)) <$$> (token LBRACKET *> sep_by_trailing (token COMMA) expr <* token RBRACKET))(* [ e, .., e ] *)
  <|> ((fun loc e p draw_expr clauses -> ListComp(loc, e, DrawClause(p, draw_expr) :: clauses))                             (* [ e | p <- e, .. ] *)
    <$$> token LBRACKET 
     *> expr 
    <*  token PIPE 
    <*> pattern 
    <*  token LARROW 
    <*> expr
    <*> list_comp_clauses
     <* token RBRACKET)
  <|> ((fun loc entries -> MapLit(loc, entries))                                                                            (* { x : e, .., x : e } *)
    <$$> token HASHLBRACE 
      *> sep_by_trailing (token COMMA) ((fun x y -> (x, y)) <$> ident <* token COLON <*> expr)
     <*  token RBRACE)
  <|> (token LPAREN *> expr <* token RPAREN)                                                                                (* ( e ) *)
  <|> ((fun loc p e -> Lambda(loc, [p], e)) <$$> token LAMBDA *> pattern <* token ARROW <*> expr)                           (* \p -> e *)
  <|> ((fun loc ps e -> Lambda(loc, ps, e)) <$$> token LAMBDA *> token LPAREN *> sep_by_trailing (token COMMA) pattern <* token RPAREN <* token ARROW <*> expr) (* \(p, .., p) -> e *)
  <|> ((fun loc p e1 e2 -> Let(loc, p, e1, e2)) <$$> token LET *> pattern <* token EQUALS <*> expr <* token IN <*> expr)    (* let x = e in e *)
  <|> ((fun loc p e1 -> LetSeq(loc, p, e1))     <$$> token LET *> pattern <* token EQUALS <*> expr)                         (* let x = e *)
  <|> ((fun loc f ps e1 e2 -> LetRec(loc, f, ps, e1, e2)) 
    <$$> token LET *> ident 
      <* token LPAREN <*> sep_by_trailing (token COMMA) pattern <* token RPAREN 
      <* token EQUALS <*> expr <* token IN <*> expr)                                                                        (* let f(p, .., p) = e in e *)
      <|> ((fun loc f ps e1 -> LetRecSeq(loc, f, ps, e1)) 
    <$$> token LET *> ident 
      <* token LPAREN <*> sep_by_trailing (token COMMA) pattern <* token RPAREN 
      <* token EQUALS <*> expr)                                                                                             (* let f(p, .., p) = e *)
  <|> (let* t = token LBRACE in let* es = sep_by_trailing (some (token SEMI)) expr in let* _ = token RBRACE in pure (Seq(Token.get_loc t, es))) (* { e ; .. ; e } *)
  <|> ((fun loc x es -> ProgCall(loc, x, es)) <$$> bang <*> many expr_leaf)                                                      (* !x e .. e *)
  <|> (token NOT *> ((fun l e -> Not(l, e)) <$$> expr_leaf))                                                                (* not e *)
  <|> ((fun loc min max -> Range(loc, min, max)) <$$> token LBRACKET *> expr <* token DDOT <*> expr <* token RBRACKET)      (* [ e .. e ] *)
  <|> ((fun loc c th el -> If(loc, c, th, el)) 
    <$$> token IF *> expr <* many (token SEMI) 
     <* token THEN <*> expr <* many (token SEMI) 
     <* token ELSE <*> expr)         (* if e then e else e *)
  (* TODO: Pipes *)
  <|> ((fun loc e -> Async(loc, e)) <$$> token ASYNC *> expr)                                                               (* async e *)
  <|> ((fun loc e -> Await(loc, e)) <$$> token AWAIT *> expr)                                                               (* await e *)
  <|> ((fun loc e branches -> Match(loc, e, branches)) 
    <$$> token MATCH 
      *> expr 
     <*  token LBRACE 
     <*> sep_by_trailing (some (token SEMI))
        ((fun x y -> (x, y)) <$> pattern <* token ARROW <*> expr)
     <*  token RBRACE)
  end stream

and list_comp_clauses stream = begin
  let list_comp_clause = 
        ((fun p e -> DrawClause(p, e)) <$> pattern <* token LARROW <*> expr)
    <|> ((fun e -> FilterClause e) <$> expr)
  in

  (token COMMA *> sep_by_trailing (token COMMA) list_comp_clause)
  <|> pure []
end stream

let entry = (fun h exprs -> (h, exprs) )
  <$> many (token SEMI)
   *> header
  <*  many (token SEMI)
  <*> sep_by_trailing (some (token SEMI)) expr


let main : Token.t Athena.Stream.t -> (header * expr list, AthenaInst.parse_error) result = 
  fun stream ->
    parse entry stream

