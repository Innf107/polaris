{
open Lexing
open Parser

(* Just after opening a new block (e.g. after '{'), the latest indentation_state
   is initially set to 'Opening'. Now, the first non-whitespace character after this
   Sets the indentation_state, setting it to 'Found <indentation>'
 *)
type indentation_state = Opening | Found of int

type lex_state = {
  mutable indentation_level: indentation_state list
}


let new_lex_state () = {
  indentation_level = [Found 0]
}

let buf_indentation lexbuf =
  let pos = lexbuf.lex_curr_p in
  pos.pos_cnum - pos.pos_bol

let open_block state lexbuf = 
  state.indentation_level <- Opening :: state.indentation_level

let close_block state lexbuf = 
  match state.indentation_level with
  | [] | [_] -> raise (Util.Panic ("Lexer.close_block: More blocks closed than opened"))
  | _ :: lvls ->
    state.indentation_level <- lvls

let try_semi (cont : lex_state -> lexbuf -> Parser.token) (state : lex_state) (lexbuf : lexbuf) : Parser.token =
  
  match state.indentation_level with
  | [] -> raise (Util.Panic ("Lexer.try_semi: Top level indentation block was closed"))
  | Found lvl :: _ -> 
    if buf_indentation lexbuf <= lvl then
      SEMI
    else
      cont state lexbuf
  | Opening :: lvls ->
    state.indentation_level <- Found (buf_indentation lexbuf) :: lvls;
    cont state lexbuf

}

let digit = ['0'-'9']

let newline = '\n' | "\r\n" 

rule block_indentation state = parse
| [ ' ' '\t' ]* ('#' | '#' [^'{'] [^'\n']*? (newline | eof))? { try_semi token state lexbuf }
| newline { new_line lexbuf; block_indentation state lexbuf }

and token state = parse
| [ ' ' '\t' ]           { token state lexbuf }
| newline                { new_line lexbuf; block_indentation state lexbuf }
| '#' | '#' [^'{'] [^'\n']*? (newline | eof) { new_line lexbuf; block_indentation state lexbuf } (* TODO: Correctly handle \r\n *)
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
| "usage" { USAGE }
| "description" { DESCRIPTION }
| "options" { OPTIONS }
| "as"      { AS }
| "->"      { ARROW }
| "<-"      { LARROW }
| ','       { COMMA }
| ';'       { SEMI }
| ':'       { COLON }
| '('       { LPAREN }
| ')'       { RPAREN }
| "#{"      { open_block state lexbuf; HASHLBRACE }
| '{'       { open_block state lexbuf; LBRACE }
| '}'       { close_block state lexbuf; RBRACE }
| '['       { open_block state lexbuf; LBRACKET }
| ']'       { close_block state lexbuf; RBRACKET }
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

