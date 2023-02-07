open Util
open Lexing

type lex_error =
  | InvalidOperator of Syntax.loc * string
  | InvalidChar of Syntax.loc * char
  | UnterminatedString

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
  | InConstructor of string
  | InOp of string
  | InString of string
  | InSingleString of string
  | InBangStart
  | InProgCall of string
  | InEnv of string
  | InNumber of string
  | InDecimal of string
  | Defer of Parser.token list
  | LeadingWhitespace

type lex_state = {
  mutable indentation_level: indentation_state list
; mutable lex_kind: lex_kind
}

let new_lex_state () = {
  indentation_level = [Found 0]
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
    if char = '\n' then
      new_line lexbuf
    else
      ();
    Some char

let set_state indentation_state state lexbuf =
  state.lex_kind <- indentation_state

let skip_location lexbuf =
  lexbuf.lex_start_p <- lexbuf.lex_curr_p;
  lexbuf.lex_start_pos <- lexbuf.lex_curr_pos

let get_loc (lexbuf : lexbuf) : Syntax.loc =
  Loc.from_pos (lexbuf.lex_curr_p) (lexbuf.lex_curr_p)

let open_block state = 
  state.indentation_level <- Opening :: state.indentation_level

let open_ignored_block state =
  state.indentation_level <- Ignored :: state.indentation_level

let close_block state =
  match state.indentation_level with
  | [] | [_] -> raise (Panic "Lexer.close_block: More blocks closed than opened")
  | _ :: lvls -> state.indentation_level <- lvls

let insert_semi (continue : unit -> Parser.token) state indentation =
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


(* character classes *)
let string_of_char = String.make 1

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_alpha_num c = is_alpha c || is_digit c

let is_ident_start c = is_alpha c || c = '_'

let is_ident c = is_ident_start c || is_digit c

let is_constructor_start c = is_ident_start c && Base.Char.is_uppercase c

let is_constructor = is_ident

let is_op_start = function
  | '=' | '<' | '>' | '+' | '*' | '/' | '&' | ',' |  '.' | '~' | ':' | ';' | '\\' | '|' -> true
  | _ -> false

let is_op c = is_op_start c || match c with
  | '-' -> true
  | _ -> false

(* TODO: This is directly adapted from the OCamllex version, but we should probably do something slightly more intelligent *)
let is_prog_char c = match c with
  | ' ' | '\t' | '\n' | '(' | ')' | '[' | ']' | '{' | '}' -> false
  | _ -> true

(* TODO: This is certainly not exhaustive. Perhaps we should allow $"var" syntax to permit arbitrary env var names? *)
let is_env_char c = match c with
  | '-' | '_' -> true
  | c when is_alpha_num c -> true
  | _ -> false

let is_paren = function
  | '(' | ')' | '[' | ']' | '{' | '}' -> true
  | _ -> false

let as_paren state = let open Parser in function
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

let ident_token = let open Parser in function
| "let" -> LET
| "in" -> IN
| "if" -> IF
| "then" -> THEN
| "else" -> ELSE
| "true" -> TRUE
| "false" -> FALSE
| "async" -> ASYNC
| "await" -> AWAIT
| "match" -> MATCH
| "usage" -> USAGE
| "description" -> DESCRIPTION
| "options" -> OPTIONS
| "as" -> AS
| "not" -> NOT
| "with" -> WITH
| "extend" -> EXTEND
| "module" -> MODULE
| "import" -> IMPORT
| "export" -> EXPORT
| "forall" -> FORALL
| "data" -> DATA
| "type" -> TYPE
| "ref" -> REF
| str -> IDENT(str)

let op_token lexbuf = let open Parser in function
| "->" -> ARROW
| "<-" -> LARROW
| "," -> COMMA
| ";" -> SEMI
| "::" -> DOUBLECOLON
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


let token (state : lex_state) (lexbuf : lexbuf): Parser.token =
  skip_location lexbuf;
  let rec go state = 
    let continue () = go state in
    match state.lex_kind with
    | Default -> 
      begin match next_char lexbuf with
      | Some('#') ->
        set_state LeadingHash state lexbuf;
        continue ()
      | Some('\n') ->
        set_state LeadingWhitespace state lexbuf;
        skip_location lexbuf;
        continue ()
      | Some(' ') ->
        skip_location lexbuf;
        continue ()
      | Some('"') ->
        set_state (InString "") state lexbuf;
        continue ()
      | Some('\'') ->
        set_state (InSingleString "") state lexbuf;
        continue ()
      | Some('`') ->
        Parser.BACKTICK
      | Some('!') ->
        set_state InBangStart state lexbuf;
        continue ()
      | Some('$') ->
        set_state (InEnv "") state lexbuf;
        continue ()
      | Some('-') ->
        set_state LeadingMinus state lexbuf;
        continue ()
      | Some(c) when is_digit c ->
        set_state (InNumber (string_of_char c)) state lexbuf;
        continue ()
      | Some(c) when is_constructor_start c ->
        set_state (InConstructor (string_of_char c)) state lexbuf;
        continue ()  
      | Some(c) when is_ident_start c ->
        set_state (InIdent (string_of_char c)) state lexbuf;
        continue ()
      | Some(c) when is_op_start c ->
        set_state (InOp (string_of_char c)) state lexbuf;
        continue ()
      | Some(c) when is_paren c ->
        as_paren state c
      | None -> Parser.EOF
      | Some(c) -> raise (LexError (InvalidChar (get_loc lexbuf, c)))
      end
      (* TODO: Is this still necessary now that there is no '#{' token anymore? *)
    | LeadingHash ->
      begin match next_char lexbuf with
      | Some('\n') ->
        set_state (LeadingWhitespace) state lexbuf;
        skip_location lexbuf;
        continue ()
      | None ->
        Parser.EOF
      | _ ->
        set_state (Comment) state lexbuf;
        continue ()
      end
    | LeadingMinus ->
      begin match peek_char lexbuf with
      | Some(c) when is_op c ->
        set_state (InOp ("-")) state lexbuf;
        continue ()
      | Some(c) when is_digit c ->
        set_state (InNumber ("-")) state lexbuf;
        continue ()
      | Some(c) ->
        set_state (Default) state lexbuf;
        MINUS
      | None ->
        set_state (Default) state lexbuf;
        MINUS
      end
    | Comment -> begin match next_char lexbuf with
      | Some('\n') ->
        set_state (LeadingWhitespace) state lexbuf;
        skip_location lexbuf;
        continue ()
      | Some(_) -> 
        continue ()
      | None ->
        Parser.EOF
      end
    | InIdent(ident) -> begin match peek_char lexbuf with
      | Some(c) when is_ident c ->
        let _ = next_char lexbuf in
        set_state (InIdent(ident ^ string_of_char c)) state lexbuf;
        continue ()
      | Some(_) ->
        set_state (Default) state lexbuf;
        ident_token ident
      | None -> 
        set_state (Default) state lexbuf;
        ident_token ident
      end
    | InConstructor(ident) -> begin match peek_char lexbuf with
      | Some(c) when is_ident c ->
        let _ = next_char lexbuf in
        set_state (InConstructor(ident ^ string_of_char c)) state lexbuf;
        continue ()
      | Some(_) ->
        set_state Default state lexbuf;
        CONSTRUCTOR ident
      | None -> 
        set_state Default state lexbuf;
        CONSTRUCTOR ident
      end
    | InString str -> begin match next_char lexbuf with
      | Some('"') ->
        set_state (Default) state lexbuf;
        STRING str
      | Some(c) ->
        set_state (InString (str ^ string_of_char c)) state lexbuf;
        continue ()
      | None ->
        raise (LexError UnterminatedString)
      end
    | InSingleString str -> 
      begin match next_char lexbuf with
      | Some('\'') ->
        set_state (Default) state lexbuf;
        STRING str
      | Some(c) ->
        set_state (InSingleString (str ^ string_of_char c)) state lexbuf;
        continue ()
      | None ->
        raise (LexError UnterminatedString)
      end
    | InBangStart -> begin match peek_char lexbuf with
      | Some('=') ->
        let _ = next_char lexbuf in
        set_state (Default) state lexbuf;
        BANGEQUALS
      | Some('.') | Some('!') ->
        set_state Default state lexbuf;
        BANG
      | Some(c) when is_prog_char c ->
        let _ = next_char lexbuf in
        set_state (InProgCall (string_of_char c)) state lexbuf;
        continue ()
      | _ ->
        set_state Default state lexbuf;
        BANG
      end
    | InProgCall str -> begin match peek_char lexbuf with
      
      | Some(c) when is_prog_char c ->
        let _ = next_char lexbuf in
        set_state (InProgCall (str ^ string_of_char c)) state lexbuf;
        continue ()
      | _ ->
        set_state (Default) state lexbuf;
        PROGCALL str
      end
    | InEnv str -> begin match peek_char lexbuf with
      | Some(c) when is_env_char c ->
        let _ = next_char lexbuf in
        set_state (InEnv (str ^ string_of_char c)) state lexbuf;
        continue ()
      | _ ->
        set_state Default state lexbuf;
        ENVVAR str
      end
    | InOp str -> begin match peek_char lexbuf with
      | Some(c) when is_op c ->
        let _ = next_char lexbuf in
        set_state (InOp (str ^ string_of_char c)) state lexbuf;
        continue ()
      | _ ->
        set_state (Default) state lexbuf;
        op_token lexbuf str
      end
    | InNumber str -> begin match peek_char lexbuf with
      | Some(c) when is_digit c ->
        let _ = next_char lexbuf in
        set_state (InNumber (str ^ string_of_char c)) state lexbuf;
        continue ()
      | Some('.') ->
        let _ = next_char lexbuf in
        begin match peek_char lexbuf with
        | Some('.') ->
          let _ = next_char lexbuf in
          set_state (Defer [DDOT]) state lexbuf;
          INT (int_of_string str)
        | _ -> 
          set_state (InDecimal (str ^ ".")) state lexbuf;
          continue ()
        end
      | _ ->
        set_state (Default) state lexbuf;
        INT (int_of_string str)
      end
    | InDecimal str -> begin match peek_char lexbuf with
      | Some(c) when is_digit c ->
        let _ = next_char lexbuf in
        set_state (InDecimal (str ^ string_of_char c)) state lexbuf;
        continue ()
      | _ ->
        set_state (Default) state lexbuf;
        FLOAT (float_of_string str)  
      end
    | LeadingWhitespace -> begin match peek_char lexbuf with
      | Some(' ' | '\n') ->
        let _ = next_char lexbuf in
        continue ()
      | Some('#') ->
        let indentation = ((get_loc lexbuf).start_col - 1) in
        let _ = next_char lexbuf in
        begin match peek_char lexbuf with
        | Some('{') ->
          set_state (LeadingHash) state lexbuf;
          insert_semi continue state indentation
        | _ ->
          set_state (LeadingHash) state lexbuf;
          continue ()
        end
      | _ ->
        set_state (Default) state lexbuf;
        insert_semi continue state ((get_loc lexbuf).start_col - 1)
      end
    | Defer [] ->
      set_state (Default) state lexbuf;
      continue ()
    | Defer (tok :: toks) ->
      set_state (Defer toks) state lexbuf;
      tok
    in
    go state
