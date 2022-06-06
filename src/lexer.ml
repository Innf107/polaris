open Util
open Lexing
open Parser

type lex_error =
  | InvalidOperator of Syntax.loc * string

exception LexError of lex_error

(* Just after opening a new block (e.g. after '{'), the latest indentation_state
   is initially set to 'Opening'. Now, the first non-whitespace character after this
   Sets the indentation_state, setting it to 'Found <indentation>'
   
   'Ignored' is used for map literals, which are also closed with '}', but should not open an indentation block
 *)
type indentation_state = Opening | Found of int | Ignored

type lex_kind = 
  | Default 
  | LeadingHash 
  | LeadingMinus
  | Comment 
  | InIdent of string
  | InOp of string
  | InString of string
  | InBang of string
  | InNumber of string
  | InDecimal of string
  | Defer of token list
  | LeadingWhitespace

type lex_state = {
  mutable indentation_level: indentation_state list
; mutable lex_kind: lex_kind
}

let new_lex_state () = {
  indentation_level = [Opening]
; lex_kind = Default
}


(* lexbuf manipulation *)

let peek_char (lexbuf : lexbuf) : char option = 
  lexbuf.refill_buff lexbuf; (* TODO: Should we really refill every time? *)
  if lexbuf.lex_curr_pos >= lexbuf.lex_buffer_len then
    None
  else
    let char = Bytes.get lexbuf.lex_buffer lexbuf.lex_curr_pos in
    Some char


let next_char (lexbuf : lexbuf) : char option =
  match peek_char lexbuf with
  | None -> None
  | Some(char) ->
    lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos + 1;
    lexbuf.lex_curr_p <- { 
      lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_p.pos_cnum + 1
    };
    if char == '\n' then
      new_line lexbuf
    else
      ();
    Some char


let get_loc (lexbuf : lexbuf) : Syntax.loc =
  Syntax.Loc.from_pos (lexbuf.lex_curr_p) (lexbuf.lex_curr_p)

let open_block state = 
  state.indentation_level <- Opening :: state.indentation_level

let open_ignored_block state =
  state.indentation_level <- Ignored :: state.indentation_level

let close_block state =
  match state.indentation_level with
  | [] | [_] -> raise (Panic "Lexer.close_block: More blocks closed than opened")
  | _ :: lvls -> state.indentation_level <- lvls

(* character classes *)
let string_of_char = String.make 1

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_alpha_num c = is_alpha c || is_digit c

let is_ident_start c = is_alpha c || c == '_'

let is_ident c = is_ident_start c || is_digit c

let is_op_start = function
  | '=' | '<' | '>' | '+' | '*' | '/' | '&' | ',' |  '.' | '~' | ':' | ';' | '\\' | '|' -> true
  | _ -> false

let is_op c = is_op_start c || match c with
  | '-' -> true
  | _ -> false

(* TODO: This is directly adapted from the OCamllex version, but we should probably to something slightly more intelligent *)
let is_prog_char c = match c with
  | ' ' | '\t' | '\n' | '(' | ')' | '[' | ']' | '{' | '}' -> false
  | _ -> true

let is_paren = function
  | '(' | ')' | '[' | ']' | '{' | '}' -> true
  | _ -> false

let as_paren state = function
  | '(' -> LPAREN
  | ')' -> RPAREN 
  | '[' -> LBRACKET
  | ']' -> RBRACKET 
  | '{' -> 
    open_block state;
    LBRACE
  | '}' -> 
    close_block state;
    RBRACE
  | c -> raise (Panic ("Lexer.as_paren: Invalid paren: '" ^ string_of_char c ^ "'"))

let ident_token = function
| "let" -> LET
| "in" -> IN
| "if" -> IF
| "then" -> THEN
| "else" -> ELSE
| "true" -> TRUE
| "false" -> FALSE
| "null" -> NULL 
| "async" -> ASYNC
| "await" -> AWAIT
| "usage" -> USAGE
| "description" -> DESCRIPTION
| "options" -> OPTIONS
| "as" -> AS
| "not" -> NOT
| str -> IDENT(str)

let op_token lexbuf = function
| "->" -> ARROW
| "<-" -> LARROW
| "," -> COMMA
| ";" -> SEMI
| ":" -> COLON
| "+" -> PLUS
| "*" -> STAR
| "/" -> SLASH
| "||" -> OR
| "&&" -> AND
| "." -> DOT
| ".." -> DDOT
| "~" -> TILDE
| "|" -> PIPE
| "=" -> EQUALS
| ":=" -> COLONEQUALS
| "==" -> DOUBLEEQUALS
| "<" -> LT
| ">" -> GT
| "<=" -> LE
| ">=" -> GE
| "\\" -> LAMBDA
| str -> raise (LexError (InvalidOperator (get_loc lexbuf, str)))


let rec token (state : lex_state) (lexbuf : lexbuf): Parser.token =
  let continue () = token state lexbuf in
  match state.lex_kind with
  | Default -> 
    begin match next_char lexbuf with
    | Some('#') ->
      state.lex_kind <- LeadingHash;
      continue ()
    | Some('\n') ->
      state.lex_kind <- LeadingWhitespace;
      continue ()
    | Some(' ') ->
      continue ()
    | Some('"') ->
      state.lex_kind <- InString "";
      continue ()
    | Some('!') ->
      state.lex_kind <- InBang "";
      continue ()
    | Some('-') ->
      state.lex_kind <- LeadingMinus;
      continue ()
    | Some(c) when is_digit c ->
      state.lex_kind <- InNumber (string_of_char c);
      continue ()
    | Some(c) when is_ident_start c ->
      state.lex_kind <- InIdent (string_of_char c);
      continue ()
    | Some(c) when is_op_start c ->
      state.lex_kind <- InOp (string_of_char c);
      continue ()
    | Some(c) when is_paren c ->
      as_paren state c
    | None -> Parser.EOF
    | Some(c) -> raise (Panic ("Default: "^ string_of_char c))
    end
  | LeadingHash ->
    begin match next_char lexbuf with
    | Some('{') ->
      open_ignored_block state;
      state.lex_kind <- Default;
      HASHLBRACE
    | Some('\n') ->
      state.lex_kind <- LeadingWhitespace;
      continue ()
    | _ ->
      state.lex_kind <- Comment;
      continue ()
    end
  | LeadingMinus ->
    begin match next_char lexbuf with
    | Some(' ' | '\t') ->
      state.lex_kind <- Default;
      MINUS
    | Some('\n') ->
      state.lex_kind <- LeadingWhitespace;
      MINUS
    | Some(c) when is_op c ->
      state.lex_kind <- InOp ("-" ^ string_of_char c);
      continue ()
    | Some(c) when is_digit c ->
      state.lex_kind <- InNumber ("-" ^ string_of_char c);
      continue ()
    | _ -> todo __POS__
    end
  | Comment -> begin match next_char lexbuf with
    | Some('\n') ->
      state.lex_kind <- Default;
      continue ()
    | _ -> 
      continue()
    end
  | InIdent(ident) -> begin match next_char lexbuf with
    | Some(c) when is_ident c ->
      state.lex_kind <- InIdent(ident ^ string_of_char c);
      continue ()
    | Some(' ' | '\t') ->
      state.lex_kind <- Default;
      ident_token ident
    | Some('\n') ->
      state.lex_kind <- LeadingWhitespace;
      ident_token ident
    | Some(c) when is_paren c ->
      state.lex_kind <- Defer [as_paren state c];
      ident_token ident
    | Some(c) when is_op_start c ->
      state.lex_kind <- InOp (string_of_char c);
      ident_token ident
    | None -> 
      state.lex_kind <- Default;
      ident_token ident
    | _ -> todo __POS__
    end
  | InString str -> begin match next_char lexbuf with
    | Some('"') ->
      state.lex_kind <- Default;
      STRING str
    | Some(c) ->
      state.lex_kind <- InString (str ^ string_of_char c);
      continue ()
    | None -> todo __POS__
    end
  | InBang str -> begin match next_char lexbuf with
    | Some(' ' | '\t') ->
      state.lex_kind <- Default;
      BANG str
    | Some('\n') ->
      state.lex_kind <- LeadingWhitespace;
      BANG str
    | Some('=') ->
      state.lex_kind <- Default;
      BANGEQUALS
    | Some(c) when is_prog_char c ->
      state.lex_kind <- InBang (str ^ string_of_char c);
      continue ()
    | _ -> todo __POS__
    end
  | InOp str -> begin match next_char lexbuf with
    | Some(c) when is_op c ->
      state.lex_kind <- InOp (str ^ string_of_char c);
      continue ()
    | Some(' ' | '\t') ->
      state.lex_kind <- Default;
      op_token lexbuf str
    | Some('\n') ->
      state.lex_kind <- LeadingWhitespace;
      op_token lexbuf str
    | Some(c) when is_ident_start c ->
      state.lex_kind <- InIdent (string_of_char c);
      op_token lexbuf str
    | Some(c) when is_paren c ->
      state.lex_kind <- Defer [as_paren state c];
      op_token lexbuf str
    | Some(c) when is_digit c ->
      state.lex_kind <- InNumber (string_of_char c);
      op_token lexbuf str
    | Some('"') ->
      state.lex_kind <- InString "";
      op_token lexbuf str
    | _ -> todo __POS__
    end
  | InNumber str -> begin match next_char lexbuf with
    | Some(c) when is_digit c ->
      state.lex_kind <- InNumber (str ^ string_of_char c);
      continue ()
    | Some('.') ->
      state.lex_kind <- InDecimal (str ^ ".");
      continue ()
    | Some(' ' | '\t') ->
      state.lex_kind <- Default;
      INT (int_of_string str)
    | Some('\n') ->
      state.lex_kind <- LeadingWhitespace;
      INT (int_of_string str)
    | Some(c) when is_paren c ->
      state.lex_kind <- Defer [as_paren state c];
      INT (int_of_string str)
    | Some('-') ->
      state.lex_kind <- LeadingMinus;
      INT (int_of_string str)
    | Some('#') ->
      state.lex_kind <- LeadingHash;
      INT (int_of_string str)
    | Some(c) when is_op_start c ->
      state.lex_kind <- InOp (string_of_char c);
      INT (int_of_string str)
    | None ->
      state.lex_kind <- Default;
      INT (int_of_string str)
    | Some(c) -> raise (Panic ("InNumber: " ^ string_of_char c))
    end
  | InDecimal str -> begin match next_char lexbuf with
    | Some(c) when is_digit c ->
      state.lex_kind <- InDecimal (str ^ string_of_char c);
      continue ()
    | Some(' ' | '\t') ->
      state.lex_kind <- Default;
      FLOAT (float_of_string str)
    | Some('\n') ->
      state.lex_kind <- LeadingWhitespace;
      FLOAT (float_of_string str)
    | Some(c) when is_paren c ->
      state.lex_kind <- Defer [as_paren state c];
      FLOAT (float_of_string str)  
    | _ -> todo __POS__
    end
  | LeadingWhitespace -> begin match peek_char lexbuf with
    | Some(' ' | '\n') ->
      let _ = next_char lexbuf in
      continue ()
    | _ ->
      state.lex_kind <- Default;
      let indentation = (get_loc lexbuf).start_col - 1 in
      match state.indentation_level with
      | (Opening :: lvls) ->
        state.indentation_level <- Found indentation :: lvls;
        continue ()
      | (Found block_indentation :: lvls) ->
        if indentation <= block_indentation then
          SEMI
        else
          continue ()
      | (Ignored :: lvls) ->
        continue ()
      | [] -> raise (Panic "Lexer: LeadingWhitespace: More blocks closed than opened")
    end
  | Defer [] ->
    state.lex_kind <- Default;
    continue ()
  | Defer (tok :: toks) ->
    state.lex_kind <- Defer toks;
    tok

