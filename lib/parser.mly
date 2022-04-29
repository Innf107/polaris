%{
open Ast
open StringExpr

%}

%token <string> IDENT
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token LET IN
%token LAMBDA ARROW
%token COMMA SEMI
%token LPAREN RPAREN LBRACE RBRACE
%token PLUS MINUS STAR SLASH
%token EQUALS
%token UNIT
%token EOF

%left PLUS MINUS
%left STAR SLASH

%start main

%type <expr list> main

%%
main:
    expr_list EOF { $1 }
;

expr:
  | STRING                                                  { StringLit $1 }                // "str"
  | INT                                                     { IntLit $1 }                   // n
  | FLOAT                                                   { FloatLit $1 }                 // f
  | UNIT                                                    { UnitLit }                     // ()
  | IDENT                                                   { Var $1 }                      // x
  | LPAREN expr RPAREN                                      { $2 }                          // ( e )
  | LAMBDA IDENT ARROW expr                                 { Lambda([$2], $4) }            // \x -> e
  | LAMBDA LPAREN ident_list RPAREN ARROW expr              { Lambda($3, $6) }              // \(x, .., x) -> e
  | LET IDENT EQUALS expr IN expr                           { Let ($2, $4, $6) }            // let x = e in e
  | LET IDENT EQUALS expr                                   { LetSeq ($2, $4) }             // let x = e
  | LET IDENT LPAREN ident_list RPAREN EQUALS expr          { LetSeq ($2, Lambda($4, $7))}  // let f(x, .., x) = e
  | LET IDENT LPAREN ident_list RPAREN EQUALS expr IN expr  { Let ($2, Lambda($4, $7), $9)} // let f(x, .., x) = e in e
  | LBRACE expr_list RBRACE                                 { Seq($2) }                     // {e; ..; e}
  | expr LPAREN expr_list RPAREN                            { App($1, $3)    }              // e(e,..,e)
  | IDENT EQUALS expr                                       { Assign($1, $3)}               // e = e
  | SLASH IDENT exprs                                       { ProgCall($2, $3) }            // /p e .. e

exprs:
  | expr expr_list { $1 :: $2 }
  | { [] }

expr_list:
  | expr SEMI expr_list { $1 :: $3 }
  | expr { [$1] }
  | { [] }

ident_list:
  | IDENT COMMA ident_list { $1 :: $3 }
  | IDENT { [$1] }
  | { [] }
%%
