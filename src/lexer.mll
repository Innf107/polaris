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
| '#' | '#' [^'{'] [^'\n']*? (newline | eof) { next_line lexbuf; token lexbuf } (* TODO: Correctly handle \r\n *)
| '-'? digit+ as lit_string { INT (int_of_string lit_string)}
| '-'? digit+ '.' digit+ as lit_string { FLOAT (float_of_string lit_string) }
| '!' '"' ([^'"']+ as cmd)'"' { BANG cmd }
| "!="      { BANGEQUALS }
| '!' ([^' ' '\t' '\n' '(' ')' '[' ']' '{' '}']+ as cmd) { BANG cmd}
| "let"     { LET }
| "in"      { IN }
| "if"      { IF }
| "then"    { THEN }
| "else"    { ELSE }
| "true"    { TRUE }
| "false"   { FALSE }
| "null"    { NULL }
| "async"   { ASYNC }
| "await"   { AWAIT }
| "->"      { ARROW }
| "<-"      { LARROW }
| ','       { COMMA }
| ';'       { SEMI }
| ':'       { COLON }
| '('       { LPAREN }
| ')'       { RPAREN }
| "#{"      { HASHLBRACE }
| '{'       { LBRACE }
| '}'       { RBRACE }
| '['       { LBRACKET }
| ']'       { RBRACKET }
| '+'       { PLUS }
| '-'       { MINUS }
| '*'       { STAR }
| '/'       { SLASH }
| "||"      { OR }
| "&&"      { AND }
| "not"     { NOT }
| '.'       { DOT }
| ".."      { DDOT }
| '~'       { TILDE }
| "|"       { PIPE }
| '='       { EQUALS }
| ":="      { COLONEQUALS }
| "=="      { DOUBLEEQUALS }
| '<'       { LT }
| '>'       { GT }
| "<="      { LE }
| ">="      { GE }
| ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* as id { IDENT id }
| '"' ([^'"']*? as str) '"' { STRING str }
| "\\"      { LAMBDA }
| eof       { EOF }

