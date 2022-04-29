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
%token EQUALS
%token LT GT LE GE
%token PLUS MINUS STAR SLASH
%token IF THEN ELSE
%token UNIT
%token EOF

%left EQUALS LT GT LE GE
%left PLUS MINUS
%left STAR SLASH

%left EQUALS

%start main

%type <expr list> main

%%
main:
    expr_semi_list EOF { $1 }
;

expr:
  | STRING                                                  { StringLit $1 }                // "str"
  | INT                                                     { NumLit (float_of_int $1) }    // n
  | FLOAT                                                   { NumLit $1 }                   // f
  | UNIT                                                    { UnitLit }                     // ()
  | IDENT                                                   { Var $1 }                      // x
  | LPAREN expr RPAREN                                      { $2 }                          // ( e )
  | LAMBDA IDENT ARROW expr                                 { Lambda([$2], $4) }            // \x -> e
  | LAMBDA LPAREN ident_list RPAREN ARROW expr              { Lambda($3, $6) }              // \(x, .., x) -> e
  | LET IDENT EQUALS expr IN expr                           { Let ($2, $4, $6) }            // let x = e in e
  | LET IDENT EQUALS expr                                   { LetSeq ($2, $4) }             // let x = e
  | LET IDENT LPAREN ident_list RPAREN EQUALS expr          { LetSeq ($2, Lambda($4, $7))}  // let f(x, .., x) = e
  | LET IDENT LPAREN ident_list RPAREN EQUALS expr IN expr  { Let ($2, Lambda($4, $7), $9)} // let f(x, .., x) = e in e
  | LBRACE expr_comma_list RBRACE                           { Seq($2) }                     // {e; ..; e}
  | expr LPAREN expr_comma_list RPAREN                      { App($1, $3)    }              // e(e,..,e)
  | IDENT EQUALS expr                                       { Assign($1, $3)}               // x = e
  | SLASH IDENT exprs                                       { ProgCall($2, $3) }            // /p e .. e
  | expr PLUS expr                                          { Add($1, $3) }                 // e + e
  | expr MINUS expr                                         { Sub($1, $3) }                 // e - e
  | expr STAR expr                                          { Mul($1, $3) }                 // e * e
  | expr SLASH expr                                         { Div($1, $3) }                 // e + e
  | expr EQUALS expr                                        { Equals($1, $3) }              // e = e
  | expr LT expr                                            { LT($1, $3)  }                 // e < e
  | expr GT expr                                            { GT($1, $3)  }                 // e > e
  | expr LE expr                                            { LE($1, $3)  }                 // e <= e
  | expr GE expr                                            { GE($1, $3)  }                 // e >= e
  | IF expr THEN expr ELSE expr                             { If($2, $4, $6) }              // if e then e else e
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
