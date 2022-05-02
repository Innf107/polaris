{
open Lexing
open Parser

exception LexError of string


let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let digit = ['0'-'9']

let newline = '\n' | "\r\n" 

rule token = parse
| [ ' ' '\t' ]           { token lexbuf }
| newline                { next_line lexbuf; token lexbuf }
| "#" [^'\n']*? (newline | eof) { token lexbuf } (* TODO: Correctly handle \r\n *)
| '-'? digit+ '.' digit* as lit_string { FLOAT (float_of_string lit_string) }
| '-'? digit+ as lit_string { INT (int_of_string lit_string)}
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
| ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* as id { IDENT id }
| '"' ([^'"']*? as str) '"' { STRING str }
| "\\"      { LAMBDA }
| eof       { EOF }

