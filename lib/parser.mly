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
%token LET REC IN
%token LAMBDA ARROW
%token COMMA SEMI
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token EQUALS
%token DOUBLEEQUALS LT GT LE GE
%token PLUS MINUS STAR SLASH
%token <string> BANG
%token IF THEN ELSE
%token EOF

%left EQUALS
%left DOUBLEEQUALS LT GT LE GE
%left PLUS MINUS
%left STAR SLASH

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
  | IDENT                                                       { Var (loc $startpos $endpos, $1) }                                // x
  | LBRACKET expr_comma_list RBRACKET                           { ListLit (loc $startpos $endpos, $2) }
  | LPAREN expr RPAREN                                          { $2 }                                                             // ( e )
  | LAMBDA IDENT ARROW expr                                     { Lambda(loc $startpos $endpos, [$2], $4) }                        // \x -> e
  | LAMBDA LPAREN ident_list RPAREN ARROW expr                  { Lambda(loc $startpos $endpos, $3, $6) }                          // \(x, .., x) -> e
  | LET IDENT EQUALS expr IN expr                               { Let (loc $startpos $endpos, $2, $4, $6) }                        // let x = e in e
  | LET IDENT EQUALS expr                                       { LetSeq (loc $startpos $endpos, $2, $4) }                         // let x = e
  | LET IDENT LPAREN ident_list RPAREN EQUALS expr              { LetSeq (loc $startpos $endpos, $2, Lambda(loc $startpos $endpos, $4, $7)) }             // let f(x, .., x) = e
  | LET REC IDENT LPAREN ident_list RPAREN EQUALS expr          { LetRecSeq (loc $startpos $endpos, $3, $5, $8) }                  // let rec f(x, .., x) = e
  | LET IDENT LPAREN ident_list RPAREN EQUALS expr IN expr      { Let (loc $startpos $endpos, $2, Lambda(loc $startpos $endpos, $4, $7), $9)}             // let f(x, .., x) = e in e
  | LET REC IDENT LPAREN ident_list RPAREN EQUALS expr IN expr  { LetRec (loc $startpos $endpos, $3, $5, $8, $10) }                // let rec f(x, .., x) = e in e
  | LBRACE expr_semi_list RBRACE                                { Seq(loc $startpos $endpos, $2) }                                 // {e; ..; e}
  | expr LPAREN expr_comma_list RPAREN                          { match $1 with 
                                                                  | Var (_, "print") -> Print(loc $startpos $endpos, List.hd $3) (* print(e) *)
                                                                  | _ -> App(loc $startpos $endpos, $1, $3) }                      // e(e,..,e)
  | IDENT EQUALS expr                                           { Assign(loc $startpos $endpos, $1, $3)}                           // x = e
  | BANG exprs                                                  { ProgCall(loc $startpos $endpos, $1, $2) }                        // /p e .. e
  | expr PLUS expr                                              { Add(loc $startpos $endpos, $1, $3) }                             // e + e
  | expr MINUS expr                                             { Sub(loc $startpos $endpos, $1, $3) }                             // e - e
  | expr STAR expr                                              { Mul(loc $startpos $endpos, $1, $3) }                             // e * e
  | expr SLASH expr                                             { Div(loc $startpos $endpos, $1, $3) }                             // e + e
  | expr DOUBLEEQUALS expr                                      { Equals(loc $startpos $endpos, $1, $3) }                          // e = e
  | expr LT expr                                                { LT(loc $startpos $endpos, $1, $3)  }                             // e < e
  | expr GT expr                                                { GT(loc $startpos $endpos, $1, $3)  }                             // e > e
  | expr LE expr                                                { LE(loc $startpos $endpos, $1, $3)  }                             // e <= e
  | expr GE expr                                                { GE(loc $startpos $endpos, $1, $3)  }                             // e >= e
  | IF expr THEN expr ELSE expr                                 { If(loc $startpos $endpos, $2, $4, $6) }                          // if e then e else e
  // TODO: Pipes

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

ident_list:
  | IDENT COMMA ident_list { $1 :: $3 }
  | IDENT { [$1] }
  | { [] }
%%

