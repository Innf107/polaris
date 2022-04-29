{
open Parser
}

rule token = parse
| [' ' '\t' '\n'] { token lexbuf }
| "#" _* '\n' { token lexbuf }
| '-'? ['0'-'9']+ '.' ['0'-'9']* as lit_string { FLOAT (float_of_string lit_string) }
| '-'? ['0'-'9']+ as lit_string { INT (int_of_string lit_string)}
| "let"     { LET }
| "rec"     { REC }
| "in"      { IN }
| "if"      { IF }
| "then"    { THEN }
| "else"    { ELSE }
| "->"      { ARROW }
| ','       { COMMA }
| ';'       { SEMI }
| '('       { LPAREN }
| ')'       { RPAREN }
| '{'       { LBRACE }
| '}'       { RBRACE }
| '+'       { PLUS }
| '-'       { MINUS }
| '*'       { STAR }
| '/'       { SLASH }
| '='       { EQUALS }
| "=="      { DOUBLEEQUALS }
| '<'       { LT }
| '>'       { GT }
| "<="      { LE }
| ">="      { GE }
| "()"      { UNIT }
| ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* as id { IDENT id }
| '"' (_* as str) '"' { STRING str }
| "\\"      { LAMBDA }
| eof       { EOF }
