%{
open Ast
open StringExpr
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil

let loc = Loc.from_pos

%}

%token <string> IDENT
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token LET IN
%token TRUE FALSE
%token NULL
%token LAMBDA ARROW
%token LARROW
%token DOT
%token COMMA SEMI COLON
%token LPAREN RPAREN HASHLBRACE LBRACE RBRACE LBRACKET RBRACKET
%token EQUALS COLONEQUALS
%token BANGEQUALS DOUBLEEQUALS LT GT LE GE
%token PLUS MINUS STAR SLASH TILDE
%token DDOT
%token OR AND NOT
%token <string> BANG
%token PIPE
%token IF THEN ELSE
%token ASYNC AWAIT
%token EOF

%left OR AND NOT
%left BANGEQUALS DOUBLEEQUALS LT GT LE GE
%left PLUS MINUS
%left STAR SLASH
%right TILDE

%nonassoc PIPE

%start main

%type <expr list> main

%%
main:
    expr_semi_list EOF { $1 }

;

expr:
  | STRING                                                      { StringLit (loc $startpos $endpos, $1) }                          // "str"
  | INT                                                         { NumLit (loc $startpos $endpos, float_of_int $1) }                // n
  | FLOAT                                                       { NumLit (loc $startpos $endpos, $1) }                             // f
  | LPAREN RPAREN                                               { UnitLit (loc $startpos $endpos) }                                // ()
  | TRUE                                                        { BoolLit (loc $startpos $endpos, true)}                           // true
  | FALSE                                                       { BoolLit (loc $startpos $endpos, false)}                          // false
  | NULL                                                        { NullLit (loc $startpos $endpos) }                                // null
  | IDENT                                                       { Var (loc $startpos $endpos, $1) }                                // x
  | LBRACKET expr_comma_list RBRACKET                           { ListLit (loc $startpos $endpos, $2) }                            // [e, .., e]

  (* The first element has to be a draw clause to differentiate 
  between list comprehensions and pipes *)
  | LBRACKET expr PIPE IDENT LARROW expr list_comp_list RBRACKET       { ListComp (loc $startpos $endpos, $2, DrawClause ($4, $6) :: $7) } // [ e | x <- e, ... ]
  (* We need a special case for async/await expressions in list comprehensions*)
  | LBRACKET ASYNC expr PIPE IDENT LARROW expr list_comp_list RBRACKET { ListComp (loc $startpos $endpos, Async(loc $startpos $endpos, $3), DrawClause ($5, $7) :: $8)} // [ async e | x <- e, ... ]
  | LBRACKET AWAIT expr PIPE IDENT LARROW expr list_comp_list RBRACKET { ListComp (loc $startpos $endpos, Await(loc $startpos $endpos, $3), DrawClause ($5, $7) :: $8)} // [ async e | x <- e, ... ]

  | HASHLBRACE map_kv_list RBRACE                               { MapLit (loc $startpos $endpos, $2) }                             // #{ x: e, .., x: e }
  | expr DOT IDENT                                              { MapLookup (loc $startpos $endpos, $1, $3) }                      // e.x
  | expr LBRACKET expr RBRACKET                                 { DynLookup (loc $startpos $endpos, $1, $3) }                      // e[e]
  | LPAREN expr RPAREN                                          { $2 }                                                             // ( e )
  | LAMBDA IDENT ARROW expr                                     { Lambda(loc $startpos $endpos, [$2], $4) }                        // \x -> e
  | LAMBDA LPAREN ident_list RPAREN ARROW expr                  { Lambda(loc $startpos $endpos, $3, $6) }                          // \(x, .., x) -> e
  | LET IDENT EQUALS expr IN expr                               { Let (loc $startpos $endpos, $2, $4, $6) }                        // let x = e in e
  | LET IDENT EQUALS expr                                       { LetSeq (loc $startpos $endpos, $2, $4) }                         // let x = e
  | LET IDENT LPAREN ident_list RPAREN EQUALS expr              { LetRecSeq (loc $startpos $endpos, $2, $4, $7) }                  // let rec f(x, .., x) = e
  | LET IDENT LPAREN ident_list RPAREN EQUALS expr IN expr      { LetRec (loc $startpos $endpos, $2, $4, $7, $9) }                 // let rec f(x, .., x) = e in e
  | LBRACE expr_semi_list RBRACE                                { Seq(loc $startpos $endpos, $2) }                                 // {e; ..; e}
  | expr LPAREN expr_comma_list RPAREN                          { match $1 with 
                                                                  | Var (_, "print") -> Print(loc $startpos $endpos, List.hd $3) (* print(e) *)
                                                                  | _ -> App(loc $startpos $endpos, $1, $3) }                      // e(e,..,e)
  | IDENT COLONEQUALS expr                                      { Assign(loc $startpos $endpos, $1, $3)}                           // x = e
  | BANG exprs                                                  { ProgCall(loc $startpos $endpos, $1, $2) }                        // /p e .. e
  | expr PLUS expr                                              { Add(loc $startpos $endpos, $1, $3) }                             // e + e
  | expr MINUS expr                                             { Sub(loc $startpos $endpos, $1, $3) }                             // e - e
  | expr STAR expr                                              { Mul(loc $startpos $endpos, $1, $3) }                             // e * e
  | expr SLASH expr                                             { Div(loc $startpos $endpos, $1, $3) }                             // e + e
  | expr TILDE expr                                             { Concat(loc $startpos $endpos, $1, $3) }                          // e ~ e
  | expr DOUBLEEQUALS expr                                      { Equals(loc $startpos $endpos, $1, $3) }                          // e == e
  | expr BANGEQUALS expr                                        { NotEquals(loc $startpos $endpos, $1, $3) }                       // e != e
  | expr LT expr                                                { LT(loc $startpos $endpos, $1, $3)  }                             // e < e
  | expr GT expr                                                { GT(loc $startpos $endpos, $1, $3)  }                             // e > e
  | expr LE expr                                                { LE(loc $startpos $endpos, $1, $3)  }                             // e <= e
  | expr GE expr                                                { GE(loc $startpos $endpos, $1, $3)  }                             // e >= e
  | expr OR expr                                                { Or(loc $startpos $endpos, $1, $3)  }                             // e || e
  | expr AND expr                                               { And(loc $startpos $endpos, $1, $3) }                             // e && e
  | NOT expr                                                    { Not(loc $startpos $endpos, $2)     }                             // not e

  | LBRACKET expr DDOT expr RBRACKET                            { Range(loc $startpos $endpos, $2, $4) }

  | IF expr THEN expr ELSE expr                                 { If(loc $startpos $endpos, $2, $4, $6) }                          // if e then e else e
  | expr PIPE pipe_list                                         { Pipe(loc $startpos $endpos, $1 :: $3) }
  | ASYNC expr                                                  { Async(loc $startpos $endpos, $2) }
  | AWAIT expr                                                  { Await(loc $startpos $endpos, $2) }

pipe_list:
  | prog_call PIPE pipe_list  { $1 :: $3 }
  | prog_call                 { [$1] }

prog_call:
  | IDENT exprs { ProgCall(loc $startpos $endpos, $1, $2) }

exprs:
  | expr exprs { $1 :: $2 }
  | { [] }

expr_comma_list:
  | expr COMMA expr_comma_list { $1 :: $3 }
  | expr { [$1] }
  | { [] }

expr_semi_list:
  | expr SEMI expr_semi_list { $1 :: $3 }
  | expr { [$1] }
  | { [] }

map_kv_list:
  | IDENT COLON expr COMMA map_kv_list { ($1, $3) :: $5 }
  | IDENT COLON expr { [($1, $3)] }
  | { [] }

ident_list:
  | IDENT COMMA ident_list { $1 :: $3 }
  | IDENT { [$1] }
  | { [] }

list_comp_list:
  | COMMA list_comp_elem list_comp_list  { $2 :: $3 }
  (* trailing comma *)
  | COMMA { [] }
  | { [] }

list_comp_elem:
  | IDENT LARROW expr { DrawClause ($1, $3) }
  | expr              { FilterClause $1 }

%%

