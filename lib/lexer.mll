{
open Parser
}

rule token = parse
| [' ' '\t' '\n'] { token lexbuf }
| '-'? ['0'-'9']+ '.' ['0'-'9']* as lit_string { FLOAT (float_of_string lit_string) }
| '-'? ['0'-'9']+ as lit_string { INT (int_of_string lit_string)}
| "let" { LET }
| "in"  { IN }
| "->"  { ARROW }
| ','   { COMMA }
| ';'   { SEMI }
| '('   { LPAREN }
| ')'   { RPAREN }
| '{'   { LBRACE }
| '}'   { RBRACE }
| '+'   { PLUS }
| '-'   { MINUS }
| '*'   { STAR }
| '/'   { SLASH }
| '='   { EQUALS }
| "()"  { UNIT }
| ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* as id { IDENT id }
| '"' (_* as str) '"' { STRING str }
| "\\"  { LAMBDA }
| eof   { EOF }
