%{
open Syntax
open Parsed

module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil

let loc = Loc.from_pos


let equal_token : token -> token -> bool = (=)
%}

%token <string> IDENT
%token <string> STRING
%token <int>    INT
%token <float>  FLOAT
%token <string> BANG
%token <string> ENVVAR
%token LET IN
%token TRUE FALSE
%token LAMBDA "\\"
%token ARROW "->"
%token LARROW "<-"
%token DOT "."
%token COMMA ","
%token SEMI ";"
%token COLON ":"
%token DOUBLECOLON "::"
%token LPAREN "("
%token RPAREN ")"
%token HASHLBRACE "#{"
%token LBRACE "{" 
%token RBRACE "}"
%token LBRACKET "["
%token RBRACKET "]"
%token EQUALS "="
%token COLONEQUALS ":="
%token BANGEQUALS "!="
%token DOUBLEEQUALS "=="
%token LT "<"
%token GT ">"
%token LE "<="
%token GE ">="
%token PLUS "+"
%token MINUS "-"
%token STAR "*"
%token SLASH "/"
%token TILDE "~"
%token DDOT ".."
%token OR "||"
%token AND "&&"
%token NOT
%token PIPE "|"
%token IF THEN ELSE
%token ASYNC AWAIT
%token MATCH
%token USAGE DESCRIPTION OPTIONS AS
%token WITH EXTEND
%token MODULE IMPORT EXPORT
%token FORALL
%token EOF


%start <header * expr list> main

%%

main: 
    SEMI* header SEMI* sep_trailing(SEMI+, expr) EOF { ($2, $4) }

sep_by(sep, p):
    | { [] }
    | sep_by1(sep, p) { $1 }

sep_by1(sep, p):
    | p { [$1] }
    | p sep sep_by1(sep, p) { $1 :: $3 }

sep_trailing(sep, p):
    | { [] }
    | p { [$1] }
    | p sep sep_trailing(sep, p) { $1 :: $3 }    

sep_trailing1(sep, p):
    | p { [$1] }
    | p sep { [$1] }
    | p sep sep_trailing1(sep, p) { $1 :: $3 }

semis(p):
    p SEMI* { $1 }

export_item:
    IDENT { ExportVal (loc $startpos $endpos, $1) }

export_list:
    EXPORT "{" sep_trailing(semis(COMMA), semis(export_item)) "}" { $3 }

usage:
    USAGE ":" STRING { $3 }

description:
    DESCRIPTION ":" STRING { $3 }

descr_clause:
    COLON STRING { $2 }

named_default:
    | IDENT "=" STRING { ($1, $3) }

named_args:
    | sep_trailing1(COMMA, named_default) { NamedDefault $1 }
    | sep_trailing1(COMMA, IDENT) { Named $1 }

option_def:
    | STRING+ AS IDENT descr_clause? { { flags = $1; args = Switch $3; description = $4 } }
    | STRING+ "(" "*" ")" AS IDENT descr_clause? { { flags = $1; args = Varargs $6; description = $7 } }
    | STRING+ "(" named_args ")" descr_clause? { { flags = $1; args = $3; description = $5 } }

header_options:
    OPTIONS "{" sep_trailing(SEMI+, option_def) "}" { $3 }

header:
    | export_list SEMI* 
        { { exports = $1; usage = None; description = None; options = [] } }
    | usage? SEMI* description? SEMI* header_options? SEMI* 
        { { exports = []; usage = $1; description = $3; options = Option.value ~default:[] $5 } }


expr:
    | expr1 "|" pipe_list { Pipe (loc $startpos $endpos, $1 :: $3) }
    | expr1 { $1 }


pipe_progcall:
    | IDENT expr_leaf* { ProgCall(loc $startpos $endpos, $1, $2) }

pipe_list:
    | sep_by(PIPE, pipe_progcall) { $1 }

expr1:
    | expr1 binop1 expr2 { BinOp(loc $startpos $endpos, $1, $2, $3) }
    | expr2 { $1 }

binop1:
    | "&&" { And }
    | "||" { Or }

expr2:
    | expr2 binop2 expr3 { BinOp(loc $startpos $endpos, $1, $2, $3) }
    | expr3 { $1 }

binop2:
    | "==" { Equals }
    | "!=" { NotEquals }
    | "<"  { LT }
    | "<=" { LE }
    | ">"  { GT }
    | ">=" { GE }

// IMPORTANT: These are *RIGHT* associative (unlike most other binops)
expr3:
    | expr4 binop3 expr3 { BinOp(loc $startpos $endpos, $1, $2, $3) }
    | expr4 { $1 }

binop3:
    | "~" { Concat }
    | "::" { Cons }

expr4:
    | expr4 binop4 expr5 { BinOp(loc $startpos $endpos, $1, $2, $3) }
    | expr5 { $1 }

binop4:
    | "+" { Add }
    | "-" { Sub }

expr5:
    | expr5 binop5 expr6 { BinOp(loc $startpos $endpos, $1, $2, $3) }
    | expr6 { $1 }

binop5:
    | "*" { Mul }
    | "/" { Div }

expr6:
    | IDENT ":=" expr { Assign(loc $startpos $endpos, $1, $3) }
    | expr6 "." IDENT { Subscript(loc $startpos $endpos, $1, $3) }
    | expr6 "[" expr "]" { DynLookup(loc $startpos $endpos, $1, $3) }
    | expr6 "(" sep_trailing(COMMA, expr) ")" { App(loc $startpos $endpos, $1, $3) }
    | expr_leaf { $1 }

expr_leaf:
    | STRING { StringLit(loc $startpos $endpos, $1) }
    | INT { NumLit(loc $startpos $endpos, float_of_int $1) }
    | FLOAT { NumLit(loc $startpos $endpos, $1) }
    | "(" ")" { UnitLit(loc $startpos $endpos) }
    | TRUE { BoolLit(loc $startpos $endpos, true) }
    | FALSE { BoolLit(loc $startpos $endpos, false) }
    | IDENT { Var(loc $startpos $endpos, $1) }
    | ENVVAR { EnvVar(loc $startpos $endpos, $1) }
    | "[" expr1 "|" LET pattern "<-" expr list_comp_clauses "]" 
        { ListComp(loc $startpos $endpos, $2, DrawClause($5, $7) :: $8) }
    | "[" sep_trailing(COMMA, expr) "]" { ListLit(loc $startpos $endpos, $2) }
    | "#{" expr WITH sep_trailing1(COMMA, assign) "}" { RecordUpdate(loc $startpos $endpos, $2, $4) }
    | "#{" expr EXTEND sep_trailing1(COMMA, assign) "}" { RecordExtension(loc $startpos $endpos, $2, $4) }
    | "#{" sep_trailing(COMMA, assign) "}" { RecordLit(loc $startpos $endpos, $2) }
    | "(" expr ")" { $2 }
    | "(" expr COMMA sep_trailing(COMMA, expr) ")" { TupleLit(loc $startpos $endpos, $2 :: $4) }
    | "\\" pattern* "->" expr { Lambda(loc $startpos $endpos, $2, $4) }  
    | LET pattern "=" expr IN expr { Let(loc $startpos $endpos, $2, $4, $6) }
    | LET pattern "=" expr { LetSeq(loc $startpos $endpos, $2, $4) }
    | LET ENVVAR "=" expr IN expr { LetEnv(loc $startpos $endpos, $2, $4, $6) }
    | LET ENVVAR "=" expr { LetEnvSeq(loc $startpos $endpos, $2, $4) }
    | LET IDENT "(" sep_trailing(COMMA, pattern) ")" "=" expr IN expr { LetRec(loc $startpos $endpos, $2, $4, $7, $9) }
    | LET IDENT "(" sep_trailing(COMMA, pattern) ")" "=" expr         { LetRecSeq(loc $startpos $endpos, $2, $4, $7) }
    | "{" sep_trailing(SEMI+, expr) "}" { Seq(loc $startpos $endpos, $2) }
    | BANG expr_leaf* { ProgCall(loc $startpos $endpos, $1, $2) }
    | NOT expr_leaf { Not(loc $startpos $endpos, $2) }
    | "[" expr ".." expr "]" { Range(loc $startpos $endpos, $2, $4) }
    | IF expr SEMI* THEN SEMI* expr SEMI* ELSE SEMI* expr { If(loc $startpos $endpos, $2, $6, $10) }
    | ASYNC expr1 { Async(loc $startpos $endpos, $2) }
    | AWAIT expr1 { Await(loc $startpos $endpos, $2) }
    | MATCH expr "{" sep_trailing(SEMI+, match_branch) "}" { Match(loc $startpos $endpos, $2, $4) }
    | MODULE IDENT "=" mod_expr { LetModuleSeq(loc $startpos $endpos, $2, $4) }

match_branch:
    pattern "->" expr { ($1, $3) } 

assign:
    IDENT "=" expr { ($1, $3) }

list_comp_clause:
    | LET pattern1 "<-" expr { DrawClause($2, $4) }
    | expr { FilterClause($1) }

list_comp_clauses:
    | { [] }
    | COMMA sep_trailing(COMMA, list_comp_clause) { $2 }

pattern:
    | pattern1 "|" pattern { OrPat(loc $startpos $endpos, $1, $3) }
    | pattern1 { $1 }

pattern1:
    | pattern2 ":" ty { TypePat(loc $startpos $endpos, $1, $3) }
    | pattern2 { $1 }

pattern2:
    | pattern_leaf "::" pattern2 { ConsPat(loc $startpos $endpos, $1, $3) }
    | pattern_leaf { $1 }

pattern_leaf:
    | "[" sep_trailing(COMMA, pattern) "]" { ListPat(loc $startpos $endpos, $2) }
    | IDENT { VarPat(loc $startpos $endpos, $1) }
    | INT { NumPat(loc $startpos $endpos, float_of_int $1) }
    | FLOAT { NumPat(loc $startpos $endpos, $1) }
    | "(" pattern ")" { $2 }
    | "(" pattern COMMA sep_trailing(COMMA, pattern) ")" { TuplePat(loc $startpos $endpos, $2 :: $4) }

mod_expr:
    | IMPORT "(" STRING ")" { Import(loc $startpos $endpos, $3) }
    | IDENT { ModVar(loc $startpos $endpos, $1) }
    | mod_expr "." IDENT { SubModule(loc $startpos $endpos, $1, $3) }

ty:
    | "(" sep_trailing(COMMA, ty) ")" "->" ty               { Fun ($2, $5) }
    | ty1 "->" ty                                           { Fun ([$1], $3) }
    | "(" sep_trailing(COMMA, ty) ")"                       { Tuple(Array.of_list $2) }
    | FORALL IDENT* "." ty                                  { List.fold_right (fun a r -> Forall(a, r)) $2 $4 }
    | ty1                                                   { $1 }

ty1:
    | IDENT                                                 { match $1 with
                                                                | "Number" -> Number
                                                                | "Bool" -> Bool
                                                                | "String" -> String
                                                                | x -> TyVar(x)
                                                            }
    | IDENT "(" ty ")"                                      { match $1 with 
                                                                | "List" -> List($3) 
                                                                | "Promise" -> Promise($3)
                                                                | _ -> Util.panic __LOC__ (Loc.pretty (loc $startpos $endpos) ^ "Type constructors are not yet implemented! (Maybe you just misspelled 'List'?)")
                                                            }
    | "{" sep_trailing(COMMA, record_entry) "}"             { Record (RowClosed(Array.of_list $2)) }
    | "{" sep_trailing(COMMA, record_entry) "|" IDENT "}"   { Record (RowVar(Array.of_list $2, $4)) }

record_entry:
    IDENT ":" ty { ($1, $3) }
