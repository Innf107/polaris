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
  | ENVVAR of string
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
  | ENVVAR str -> "ENVVAR(" ^ str ^ ")"
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

let (let*) = AthenaInst.bind
let (and*) = AthenaInst.prod


let pretty_error = function
  | RemainingTokens toks -> "Unexpected remaining tokens.\nNext 10 tokens: " ^ String.concat ("\n") (List.map (fun (t, loc) -> Loc.pretty loc ^ ": " ^ Token.pretty t) (Util.take 10 (Stream.collect toks)))
  | UnexpectedEOF -> "Unexpected end of file"
  | ParseError msg -> msg
  | ParseErrorOn (msg, tok, loc) -> Loc.pretty loc ^ ": Unexpected '" ^ Token.pretty tok ^ "': " ^ msg
  | UnexpectedToken (tok, loc) -> Loc.pretty loc ^ ": Unexpected '" ^ Token.pretty tok ^ "'"

let ident = token_of begin fun loc -> function
  | Token.IDENT id -> Some(id, loc)
  | _ -> None
  end

let ident_ = fst <$> ident 

let string = token_of begin fun loc -> function
  | Token.STRING str -> Some(str, loc)
  | _ -> None
  end
let string_ = fst <$> string

let bang = token_of begin fun loc -> function
| Token.BANG str -> Some(str, loc)
| _ -> None
end

let envvar = token_of begin fun loc -> function
| Token.ENVVAR str -> Some(str, loc)
| _ -> None
end

let int = token_of begin fun loc -> function
| Token.INT i -> Some(i, loc)
| _ -> None
end

let float = token_of begin fun loc -> function
| Token.FLOAT i -> Some(i, loc)
| _ -> None
end

let descr_clause = optional (token COLON *> string_)

let named_args = 
  ((fun xs -> NamedDefault xs) 
    <$> sep_by_trailing1 (token COMMA) ((fun x y -> (x, y)) <$> ident_ <* token EQUALS <*> string_))
  <|> ((fun xs -> Named xs)
    <$> sep_by_trailing1 (token COMMA) ident_)

let option_def = 
  let* flags = some string_ in
  ((fun name descr -> { flags=flags; args=Switch name; description=descr })
    <$> token AS
     *> ident_
    <*> descr_clause)
  <|> ((fun name descr -> { flags=flags; args=Varargs name; description=descr })
    <$> token LPAREN
     *> token STAR
     *> token RPAREN
     *> token AS
     *> ident_
    <*> descr_clause)
  <|> ((fun args descr -> { flags=flags; args=args; description=descr })
    <$> token LPAREN
     *> named_args
    <*  token RPAREN
    <*> descr_clause)

let header_options = 
      token OPTIONS
   *> token LBRACE
   *> sep_by_trailing (some (token SEMI)) option_def
  <*  token RBRACE 

let header = (fun u d opts -> { usage = u; description = d; options = Option.value ~default:[] opts })
  <$> optional (token USAGE *> token COLON *> string_)       (* usage *)
  <*  many (token SEMI)
  <*> optional (token DESCRIPTION *> token COLON *> string_) (* description *)
  <*  many (token SEMI)
  <*> optional header_options                               (* options *)

let rec pattern stream = begin
  chainl1 pattern1 (let* loc = token PIPE in pure (fun p1 p2 -> OrPat(loc, p1, p2)))                          (* p | p *)
end stream

and pattern1 stream = begin
  chainl1 pattern_leaf (let* loc = token COLON in pure (fun p1 p2 -> ConsPat(loc, p1, p2)))                   (* p : p *)
end stream

and pattern_leaf stream = begin
      ((fun ls ps le -> ListPat(Loc.merge ls le, ps)) 
        <$> token LBRACKET <*> sep_by_trailing (token COMMA) pattern <*> token RBRACKET)                                (* [p, .., p] *)
  <|> ((fun (x, loc) -> VarPat(loc, x)) <$> ident)                                                                      (* x *)
  <|> ((fun (x, loc) -> NumPat(loc, float_of_int x)) <$> int)                                                           (* n *)
  <|> ((fun (x, loc) -> NumPat(loc, x)) <$> float)                                                                      (* f *)
  <|> (let* ls = token LPAREN in
      let* patterns = sep_by_trailing1 (token COMMA) pattern in 
      let* le = token RPAREN in
      match patterns with
      | [p] -> pure p
      | _ -> pure (TuplePat (Loc.merge ls le, patterns)))                                                                         (* ( p ) *)
end stream

let rec expr stream = begin 
  ((fun e loc pl -> Pipe(merge (get_loc e) (Option.value ~default:loc (Option.map get_loc (Util.last pl))), e :: pl)) 
    <$> expr1 <*> token PIPE <*> pipe_list)
  <|> expr1
end stream

and expr1 stream = begin
  let op = 
      (let* loc = token OR in pure (fun e1 e2 -> Or(loc, e1, e2)))
  <|> (let* loc = token AND in pure (fun e1 e2 -> And(loc, e1, e2))) 
  in
  chainl1 expr2 op
end stream

and pipe_list stream = begin
  sep_by (token PIPE) ((fun (x, loc) es -> ProgCall(Loc.merge loc (Option.value ~default:loc (Option.map get_loc (Util.last es))), x, es)) 
    <$> ident <*> many expr_leaf)
end stream

and expr2 stream = begin
  let op = 
      (let* loc = token BANGEQUALS in pure (fun e1 e2 -> NotEquals(loc, e1, e2)))
  <|> (let* loc = token DOUBLEEQUALS in pure (fun e1 e2 -> Equals(loc, e1, e2))) 
  <|> (let* loc = token LT in pure (fun e1 e2 -> LT(loc, e1, e2))) 
  <|> (let* loc = token GT in pure (fun e1 e2 -> GT(loc, e1, e2)))
  <|> (let* loc = token LE in pure (fun e1 e2 -> LE(loc, e1, e2))) 
  <|> (let* loc = token GE in pure (fun e1 e2 -> GE(loc, e1, e2))) 
  in
  chainl1 expr3 op
end stream
  
and expr3 stream = begin
  let op = 
      (let* loc = token PLUS in pure (fun e1 e2 -> Add(loc, e1, e2)))
  <|> (let* loc = token MINUS in pure (fun e1 e2 -> Sub(loc, e1, e2))) 
  <|> (let* loc = token TILDE in pure (fun e1 e2 -> Concat(loc, e1, e2))) 
  in
  chainl1 expr4 op
end stream

and expr4 stream = begin
  let op = 
      (let* loc = token STAR in pure (fun e1 e2 -> Mul(loc, e1, e2)))
  <|> (let* loc = token SLASH in pure (fun e1 e2 -> Div(loc, e1, e2))) 
  in
  chainl1 expr5 op
end stream

and expr5 stream = begin
  let ops : (expr -> expr) parser =
      (let* ls = token DOT in let* (x, le) = ident in pure (fun e -> Subscript(Loc.merge ls le, e, x)))
  <|> (let* ls = token LBRACKET in let* e2 = expr in let* le = token RBRACKET in pure (fun e -> DynLookup(Loc.merge ls le, e, e2)))
  <|> (let* ls = token LPAREN in let* es = sep_by_trailing (token COMMA) expr in let* le = token RPAREN in pure (fun e -> App(Loc.merge ls le, e, es)))
  in
      ((fun (x, loc) e -> Assign(loc, x, e)) <$> ident <* token COLONEQUALS <*> expr)                                         (* x := e *)
  <|> left_assoc expr_leaf ops
end stream

and expr_leaf stream = begin
      ((fun (x, loc) -> StringLit (loc, x))           <$> string)                                                             (* "str" *)
  <|> ((fun (x, loc) -> NumLit (loc, float_of_int x)) <$> int)                                                                (* n *)
  <|> ((fun (x, loc) -> NumLit (loc, x))              <$> float)                                                              (* f *)
  <|> ((fun ls le -> UnitLit (Loc.merge ls le))       <$> token LPAREN <*> token RPAREN)                                       (* () *)
  <|> ((fun loc -> BoolLit (loc, true))               <$> token TRUE)                                                         (* true *)
  <|> ((fun loc -> BoolLit (loc, false))              <$> token FALSE)                                                        (* false *)
  <|> ((fun loc -> NullLit loc)                       <$> token NULL)                                                         (* null *)
  <|> ((fun (x, loc) -> Var(loc, x))                  <$> ident)                                                              (* x *)
  <|> ((fun (x, loc) -> EnvVar(loc, x))               <$> envvar)                                                             (* $x *)
  <|> ((fun ls e p draw_expr clauses le -> ListComp(Loc.merge ls le, e, DrawClause(p, draw_expr) :: clauses))                 (* [ e | p <- e, .. ] *)
    <$> token LBRACKET 
    <*> expr1
    <*  token PIPE
    <*> pattern 
    <*  token LARROW 
    <*> expr
    <*> list_comp_clauses
    <*> token RBRACKET)
  <|> ((fun ls es le -> ListLit (Loc.merge ls le, es)) <$> token LBRACKET <*> sep_by_trailing (token COMMA) expr <*> token RBRACKET)  (* [ e, .., e ] *)
  <|> ((fun ls entries le -> RecordLit (Loc.merge ls le, entries))                                                                        (* { x : e, .., x : e } *)
    <$> token HASHLBRACE 
    <*> sep_by_trailing (token COMMA) ((fun x y -> (x, y)) <$> ident_ <* token COLON <*> expr)
    <*> token RBRACE)
  <|> (let* ls = token LPAREN in
      let* exprs = sep_by_trailing1 (token COMMA) expr in
      let* le = token RPAREN in
      match exprs with
      | [e] -> pure e
      | _ -> pure (TupleLit (Loc.merge ls le, exprs)))  
  <|> ((fun ls ps e -> Lambda(Loc.merge ls (get_loc e), ps, e))                                                                       (* \(p, .., p) -> e *)
    <$> token LAMBDA <*> token LPAREN *> sep_by_trailing (token COMMA) pattern <* token RPAREN <* token ARROW <*> expr)                                                                                                                         (* ( e, .., e ) *)
  <|> ((fun ls p e -> Lambda(Loc.merge ls (get_loc e), [p], e)) <$> token LAMBDA <*> pattern <* token ARROW <*> expr)                 (* \p -> e *)
  <|> ((fun ls p e1 e2 -> Let(Loc.merge ls (get_loc e2), p, e1, e2))                                                                  (* let x = e in e *)
    <$> token LET <*> pattern <* token EQUALS <*> expr <* token IN <*> expr)
  <|> ((fun ls (x, _) e1 e2 -> LetEnv(Loc.merge ls (get_loc e2), x, e1, e2))                                                                  (* let x = e in e *)
    <$> token LET <*> envvar <* token EQUALS <*> expr <* token IN <*> expr)                            
  <|> ((fun ls (x, _) e1 -> LetEnvSeq(Loc.merge ls (get_loc e1), x, e1))                                                                  (* let x = e in e *)
    <$> token LET <*> envvar <* token EQUALS <*> expr)                            
  <|> ((fun ls p e1 -> LetSeq(Loc.merge ls (get_loc e1), p, e1)) <$> token LET <*> pattern <* token EQUALS <*> expr)                  (* let x = e *)
  <|> ((fun ls f ps e1 e2 -> LetRec(Loc.merge ls (get_loc e2), f, ps, e1, e2))                                                        (* let f(p, .., p) = e in e *)
    <$> token LET <*> ident_
    <* token LPAREN <*> sep_by_trailing (token COMMA) pattern <* token RPAREN   
    <* token EQUALS <*> expr <* token IN <*> expr)                                                                        
  <|> ((fun ls f ps e1 -> LetRecSeq(Loc.merge ls (get_loc e1), f, ps, e1))                                                                                 (* let f(p, .., p) = e *)
    <$> token LET <*> ident_ 
    <* token LPAREN <*> sep_by_trailing (token COMMA) pattern <* token RPAREN 
    <* token EQUALS <*> expr)                                                                                             
  <|> (let* ls = token LBRACE in let* es = sep_by_trailing (some (token SEMI)) expr in let* le = token RBRACE in pure (Seq(Loc.merge ls le, es))) (* { e ; .. ; e } *)
  <|> ((fun (x, loc) es -> ProgCall(Loc.merge loc (Option.value ~default:loc (Option.map get_loc (Util.last es))), x, es))            (* !x e .. e *)
    <$> bang <*> many expr_leaf)                                                      
  <|> ((fun loc e -> Not(Loc.merge loc (get_loc e), e)) <$> token NOT <*> expr_leaf)                                                  (* not e *)
  <|> ((fun ls min max le -> Range(Loc.merge ls le, min, max)) <$> token LBRACKET <*> expr <* token DDOT <*> expr <*> token RBRACKET) (* [ e .. e ] *)
  <|> ((fun ls c th el -> If(Loc.merge ls (get_loc el), c, th, el))                                                                   (* if e then e else e *)
    <$> token IF <*> expr <* many (token SEMI)
    <* token THEN <*> expr <* many (token SEMI)
    <* token ELSE <*> expr)
  <|> ((fun loc e -> Async(Loc.merge loc (get_loc e), e)) <$> token ASYNC <*> expr1)                                                  (* async e *)
  <|> ((fun loc e -> Await(Loc.merge loc (get_loc e), e)) <$> token AWAIT <*> expr1)                                                  (* await e *)
  <|> ((fun ls e branches le -> Match(Loc.merge ls le, e, branches)) 
    <$> token MATCH 
    <*> expr 
    <*  token LBRACE 
    <*> sep_by_trailing (some (token SEMI))
        ((fun x y -> (x, y)) <$> pattern <* token ARROW <*> expr)
    <*>  token RBRACE)
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


let main : (Token.t * loc) Athena.Stream.t -> (header * expr list, AthenaInst.parse_error) result = 
  fun stream ->
    parse entry stream

