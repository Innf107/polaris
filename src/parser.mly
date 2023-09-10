%{
open Syntax
open Syntax.Parsed

open Parserprelude

module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil

let loc = Loc.from_pos

(* Workaround to get both `let x : ty = e` and `let f : ty; f(x) = e` working.
    (See the parsing rule of the same name) *)
type expr_or_fun_def_ext =
    | ExprExt of expr
    | FunExt of string * pattern list * expr

%}

%token <string> IDENT
%token <string> CONSTRUCTOR
%token <string> STRING
%token <int>    INT
%token <float>  FLOAT
%token <string> PROGCALL
%token <string> ENVVAR
%token INTERP_STRING_START
%token <string> STRING_COMPONENT
%token INTERPOLATION_START
%token INTERPOLATION_END
%token INTERP_STRING_END
%token BANG "!"
%token LET
%token TRUE FALSE
%token LAMBDA "\\"
%token ARROW "->"
%token LARROW "<-"
%token DOUBLEARROW "=>"
%token DOT "."
%token COMMA ","
%token SEMI ";"
%token COLON ":"
%token DOUBLECOLON "::"
%token LPAREN "("
%token RPAREN ")"
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
%token FORALL "forall"
%token DATA
%token TYPE
%token CLASS
%token INSTANCE
%token BACKTICK "`"
%token REF
%token EXCEPTION TRY RAISE
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

many(p):
    |           { [] }
    | p many(p) { $1 :: $2 }

semis(p):
    p SEMI* { $1 }

ident_with_loc:
    | IDENT { (loc $startpos $endpos, $1) }

export_item:
    | IDENT { ExportVal (loc $startpos $endpos, $1) }
    | CONSTRUCTOR { ExportConstructor (loc $startpos $endpos, $1) }

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
    | expr1 ":" ty { Ascription (loc $startpos $endpos, $1, $3) }
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
    | expr7 ":=" expr { Assign(loc $startpos $endpos, $1, $3) }
    | expr7 { $1 }

expr7:
    | expr7 "." ident_with_loc { let (field_loc, field) = $3 in Subscript({main = loc $startpos $endpos; subloc=field_loc}, $1, field) }
    | CONSTRUCTOR "." IDENT { ModSubscript(loc $startpos $endpos, $1, $3) }
    | CONSTRUCTOR "." CONSTRUCTOR { ModSubscriptDataCon((), loc $startpos $endpos, $1, $3) }
    | expr7 "[" expr "]" { DynLookup(loc $startpos $endpos, $1, $3) }
    | expr7 "(" sep_trailing(COMMA, expr) ")" { App(loc $startpos $endpos, $1, $3) }
    | expr7 "!" { Unwrap(loc $startpos $endpos, $1) }
    | expr_leaf { $1 }

string_component:
    | STRING_COMPONENT { StringComponent(loc $startpos $endpos, $1) }
    | INTERPOLATION_START sep_trailing1(SEMI+, expr) INTERPOLATION_END { Interpolation(loc $startpos $endpos, $2) }

expr_leaf:
    | STRING { StringLit(loc $startpos $endpos, $1) }
    | INTERP_STRING_START string_component* INTERP_STRING_END { StringInterpolation(loc $startpos $endpos, $2) }

    | INT { NumLit(loc $startpos $endpos, float_of_int $1) }
    | FLOAT { NumLit(loc $startpos $endpos, $1) }
    | "(" ")" { UnitLit(loc $startpos $endpos) }
    | TRUE { BoolLit(loc $startpos $endpos, true) }
    | FALSE { BoolLit(loc $startpos $endpos, false) }
    | CONSTRUCTOR { DataConstructor(loc $startpos $endpos, $1) }
    | "`" CONSTRUCTOR                                   { VariantConstructor(loc $startpos $endpos, $2, []) }
    | "`" CONSTRUCTOR "(" sep_trailing(COMMA, expr) ")" { VariantConstructor(loc $startpos $endpos, $2, $4) }
    | IDENT { Var(loc $startpos $endpos, $1) }
    | ENVVAR { EnvVar(loc $startpos $endpos, $1) }
    | "[" expr1 "|" LET pattern "<-" expr list_comp_clauses "]" 
        { ListComp(loc $startpos $endpos, $2, DrawClause($5, $7) :: $8) }
    | "[" sep_trailing(COMMA, expr) "]" { ListLit(loc $startpos $endpos, $2) }
    | "{" "}" { RecordLit(loc $startpos $endpos, [])}
    | "{" sep_trailing1(SEMI+, expr) "}"   { Seq(loc $startpos $endpos, $2) }
    | "{" sep_trailing1(COMMA, assign) "}" { RecordLit(loc $startpos $endpos, $2) }
    | "{" expr WITH sep_trailing1(COMMA, assign) "}" { RecordUpdate(loc $startpos $endpos, $2, $4) }
    | "{" expr EXTEND sep_trailing1(COMMA, assign) "}" { RecordExtension(loc $startpos $endpos, $2, $4) }
    | "(" expr ")" { $2 }
    | "(" expr COMMA sep_trailing(COMMA, expr) ")" { TupleLit(loc $startpos $endpos, $2 :: $4) }
    | "\\" pattern* "->" expr { Lambda(loc $startpos $endpos, $2, $4) }  
    | LET pattern "=" expr { LetSeq(loc $startpos $endpos, $2, $4) }
    | LET ENVVAR "=" expr { LetEnvSeq(loc $startpos $endpos, $2, $4) }
    | LET ident_with_loc "(" sep_trailing(COMMA, pattern) ")" "=" expr         { let var_loc, var = $2 in LetRecSeq({main = loc $startpos $endpos; subloc = var_loc }, None, var, $4, $7) }
    
    (* Workaround to get both `let x : ty = e` and `let f : ty; f(x) = e` working *)
    | LET ident_with_loc ":" ty expr_or_fun_def_ext
        {   match $5 with
            (* TODO: The location in the type pattern is a bit off *)
            | ExprExt expr -> 
                let var_loc, var = $2 in
                LetSeq(loc $startpos $endpos, TypePat (loc $startpos $endpos, VarPat(var_loc, var), $4), expr) 
            | FunExt(name, params, body) ->
                let (var_loc, var) = $2 in
                if var <> name then
                    raise (SpecificParseError (MismatchedLetName(loc $startpos $endpos, var, name)))
                else
                    LetRecSeq({ main = loc $startpos $endpos; subloc = var_loc }, Some $4, name, params, body)
        }

    | PROGCALL expr_leaf* { ProgCall(loc $startpos $endpos, $1, $2) }
    | NOT expr_leaf { Not(loc $startpos $endpos, $2) }
    | "[" expr ".." expr "]" { Range(loc $startpos $endpos, $2, $4) }
    | IF expr SEMI* THEN SEMI* expr SEMI* ELSE SEMI* expr { If(loc $startpos $endpos, $2, $6, $10) }
    | ASYNC expr1 { Async(loc $startpos $endpos, $2) }
    | AWAIT expr1 { Await(loc $startpos $endpos, $2) }
    | MATCH expr "{" sep_trailing(SEMI+, match_branch) "}" { Match(loc $startpos $endpos, $2, $4) }
    | MODULE CONSTRUCTOR "=" mod_expr { LetModuleSeq(loc $startpos $endpos, $2, $4) }
    | DATA CONSTRUCTOR "=" ty { LetDataSeq(loc $startpos $endpos, $2, [], $4) }
    | DATA CONSTRUCTOR "(" sep_trailing1(COMMA, IDENT) ")" "=" ty { LetDataSeq(loc $startpos $endpos, $2, $4, $7) }
    | TYPE CONSTRUCTOR "=" ty { LetTypeSeq(loc $startpos $endpos, $2, [], $4) }
    | TYPE CONSTRUCTOR "(" sep_trailing1(COMMA, IDENT) ")" "=" ty { LetTypeSeq(loc $startpos $endpos, $2, $4, $7) }
    | CLASS CONSTRUCTOR "(" sep_trailing1(COMMA, IDENT) ")" "{"
            sep_trailing(SEMI+, typed_ident)
        "}" { LetClassSeq(loc $startpos $endpos, $2, $4, $7) }
    | INSTANCE CONSTRUCTOR "(" sep_trailing1(COMMA, ty) ")" "{"
            sep_trailing(SEMI+, instance_def)
        "}" { LetInstanceSeq(loc $startpos $endpos, [], Tuple [||], $2, $4, $7) }
        (* TODO: Allow a version without parentheses around the entailed constraint *)
    | INSTANCE "(" "forall" many(IDENT) "." "(" ty ")" "=>" CONSTRUCTOR "(" sep_trailing1(COMMA, ty) ")" ")" "{"
            sep_trailing(SEMI+, instance_def)
        "}" { LetInstanceSeq(loc $startpos $endpos, $4, $7, $10, $12, $16) }
    | EXCEPTION CONSTRUCTOR "(" sep_trailing(COMMA, typed_ident) ")" "=" expr { LetExceptionSeq(loc $startpos $endpos, $2, $4, $7) }
    | TRY expr WITH "{" sep_trailing(SEMI+, match_branch) "}" { Try(loc $startpos $endpos, $2, $5) }
    | RAISE expr { Raise(loc $startpos $endpos, $2) }
    | REF expr { MakeRef(loc $startpos $endpos, $2) }

instance_def:
    | IDENT "(" sep_trailing(COMMA, pattern) ")" "=" expr { ((), $1, $3, $6) }

(* Workaround to get both `let x : ty = e` and `let f : ty; f(x) = e` working *)
expr_or_fun_def_ext:
    | "=" expr { ExprExt ($2) }
    | SEMI+ LET IDENT "(" sep_trailing(COMMA, pattern) ")" "=" expr { FunExt ($3, $5, $8) }

match_branch:
    pattern "->" expr { ($1, $3) } 

assign:
    IDENT "=" expr SEMI* { ($1, $3) }

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
    | pattern3 "::" pattern2 { ConsPat(loc $startpos $endpos, $1, $3) }
    | pattern3 { $1 }

pattern3:
    | pattern_leaf AS IDENT { AsPat(loc $startpos $endpos, $1, $3) }
    | pattern_leaf { $1 }

pattern_leaf:
    | "[" sep_trailing(COMMA, pattern) "]" { ListPat(loc $startpos $endpos, $2) }
    | IDENT { VarPat(loc $startpos $endpos, $1) }
    | CONSTRUCTOR "(" pattern ")" { DataPat(loc $startpos $endpos, $1, $3) }
    | CONSTRUCTOR "(" sep_trailing(COMMA, pattern) ")"      { VariantPat(loc $startpos $endpos, $1, $3) }
    | CONSTRUCTOR                                           { VariantPat(loc $startpos $endpos, $1, []) }
    | "`" CONSTRUCTOR "(" sep_trailing(COMMA, pattern) ")"  { VariantPat(loc $startpos $endpos, $2, $4) }
    | "`" CONSTRUCTOR                                       { VariantPat(loc $startpos $endpos, $2, []) }        
    | INT { NumPat(loc $startpos $endpos, float_of_int $1) }
    | FLOAT { NumPat(loc $startpos $endpos, $1) }
    | STRING { StringPat(loc $startpos $endpos, $1) }
    | "(" pattern ")" { $2 }
    | "(" pattern COMMA sep_trailing(COMMA, pattern) ")" { TuplePat(loc $startpos $endpos, $2 :: $4) }

mod_expr:
    | IMPORT "(" STRING ")" { Import(loc $startpos $endpos, $3) }
    | CONSTRUCTOR { ModVar(loc $startpos $endpos, $1) }
    | mod_expr "." CONSTRUCTOR { SubModule(loc $startpos $endpos, $1, $3) }

arrow:
    | "->" { `Function }
    | "=>" { `Constraint }

ty:
    | "(" ty "," sep_trailing(",", ty) ")" arrow ty                 { make_function $6 ($2 :: $4) $7 }
    | "(" ty ")" arrow ty                                           { make_function $4 [$2] $5 }
    | "(" ")" arrow ty                                              { make_function $3 [] $4}
    | ty2 arrow ty                                                  { make_function $2 [$1] $3 }
    | "(" ty "," sep_trailing(",", ty) ")"                          { Tuple(Array.of_list ($2 :: $4)) }
    | "forall" IDENT* "." ty                                        { List.fold_right (fun a r -> Forall(a, r)) $2 $4 }
    | "(" ty ")"                                                    { $2 }
    | ty1                                                           { $1 }

ty1:
    | "(" ")"                                               { Ty.unit }
    | ty2                                                   { $1 }

ty2:    
    | CONSTRUCTOR                                           { match $1 with
                                                                | "Number" -> Number
                                                                | "Bool" -> Bool
                                                                | "String" -> String
                                                                | "Exception" -> Exception
                                                                | _ -> TyConstructor($1, [])
                                                            }
    | CONSTRUCTOR "." CONSTRUCTOR                           { ModSubscriptTyCon((), $1, $3, []) }
    | IDENT                                                 { TyVar($1) }
    | CONSTRUCTOR "(" sep_trailing(COMMA, ty) ")"           { match $1 with 
                                                                | "List" | "Promise" | "Ref" -> 
                                                                    begin match $3 with 
                                                                    | [ty] -> begin match $1 with
                                                                        | "List" -> List(ty) 
                                                                        | "Promise" -> Promise(ty)
                                                                        | "Ref" -> Ref(ty)
                                                                        | _ -> Util.panic __LOC__ "impossible pattern match"
                                                                        end
                                                                    | _ -> Util.panic __LOC__ (Loc.pretty (loc $startpos $endpos) ^ ": Type constructor " ^ $1 ^ " should take exactly one argument.\nThis is a known bug, please do NOT report it! The compiler just needs some work to report this correctly. It is trying its best!")
                                                                    end
                                                                | _ -> TyConstructor($1, $3)
                                                            }
    | CONSTRUCTOR "." CONSTRUCTOR "(" sep_trailing(COMMA, ty) ")"   { ModSubscriptTyCon((), $1, $3, $5) }
    | "{" sep_trailing(COMMA, record_entry) "}"                     { RecordClosed(Array.of_list $2) }
    | "{" sep_trailing(COMMA, record_entry) "|" IDENT "}"           { RecordVar(Array.of_list $2, $4) }
    | "<" sep_trailing(COMMA, variant_entry) ">"                    { VariantClosed(Array.of_list $2) }
    | "<" sep_trailing(COMMA, variant_entry) "|" IDENT ">"          { VariantVar(Array.of_list $2, $4) }

record_entry:
    SEMI* IDENT ":" ty SEMI* { ($2, $4) }

variant_entry:
    | CONSTRUCTOR "(" sep_trailing(COMMA, ty) ")" { ($1, $3) }
    | CONSTRUCTOR                                 { ($1, []) }

typed_ident:
    IDENT ":" ty { ($1, $3) }
