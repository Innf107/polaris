
module AthenaInst = Athena.Make(struct 
  include String 
  let to_string x = x
end)
open AthenaInst

let lex_string str = Athena.Stream.of_list begin
  match String.split_on_char ' ' str with
  | [""] -> []
  | x -> x
  end

type expr = Var of string
          | Add of expr * expr
          | Sub of expr * expr
          | Mul of expr * expr

let rec pretty_expr = function
  | Var x -> x
  | Add (x, y) -> "(" ^ pretty_expr x ^ " + " ^ pretty_expr y ^ ")"
  | Sub (x, y) -> "(" ^ pretty_expr x ^ " - " ^ pretty_expr y ^ ")"
  | Mul (x, y) -> "(" ^ pretty_expr x ^ " * " ^ pretty_expr y ^ ")"

let pretty_parse_error = function
  | RemainingTokens toks -> "RemainingTokens: " ^ String.concat ", " (List.map (fun t -> "'" ^ t ^ "'") (Athena.Stream.collect toks))
  | UnexpectedEOF -> "UnexpectedEOF"
  | ParseError msg -> "Parse error: " ^ msg
  | ParseErrorOn (msg, tok) -> "Unexpected '" ^ tok ^ "': " ^ msg
  | UnexpectedToken tok     -> "Unexpected '" ^ tok ^ "'"


let var = (fun x -> Var x) <$> any

let rec mul_term stream = begin
  (fun x y -> Mul(x, y))
  <$> var
  <*  token "*"
  <*> term
end stream

and term stream = begin
      mul_term
  <|> var
end stream

and add_expr stream = begin
  (fun x y -> Add(x, y)) 
  <$> term
  <*  token "+"
  <*> expr
end stream

and sub_expr stream = begin 
  (fun x y -> Sub(x, y)) 
  <$> term
  <*  token "-"
  <*> expr
end stream
  

and expr stream  = begin
      add_expr
  <|> sub_expr
  <|> term
end stream

let should_be name actual expected pretty = 
  if actual = expected then
    print_endline ("\x1b[32m" ^ name ^  ": passed\x1b[0m")
  else begin
    print_endline ("\x1b[31m" ^ name ^ ": FAILED");
    print_endline ("expected: " ^ pretty expected);
    print_endline ("  actual: " ^ pretty actual ^ "\x1b[0m")
  end

let should_be_ok name actual_res expected pretty pretty_error =
  match actual_res with
  | Ok actual -> 
    should_be name expected actual pretty
  | Error err ->
    print_endline ("\x1b[31m" ^ name ^ ": ERROR: " ^ pretty_error err ^ "\x1b[0m")
  | exception Stack_overflow ->
    print_endline ("\x1b[31m" ^ name ^ ": ERROR: Stack overflow")


let should_parse name input expected = 
  should_be_ok name (parse expr (lex_string input)) expected pretty_expr pretty_parse_error


let id x = x

let () = 
  should_be_ok "pure" (parse (pure 5) (lex_string "")) 5 Int.to_string pretty_parse_error;
  should_be_ok "token" (parse (token "1") (lex_string "1")) "1" id pretty_parse_error;
  should_be_ok "any" (parse any (lex_string "x")) "x" id pretty_parse_error;
  should_be_ok "map id" (parse (map id any) (lex_string "x")) "x" id pretty_parse_error;
  should_be_ok "map Var" (parse ((fun x -> Var x) <$> any) (lex_string "x")) (Var "x") pretty_expr pretty_parse_error;
  should_be_ok "var" (parse var (lex_string "x")) (Var "x") pretty_expr pretty_parse_error;
  should_be_ok "var var" (parse (var *> var) (lex_string "x y")) (Var "y") pretty_expr pretty_parse_error;
  should_be_ok "many" (parse (many var) (lex_string "x y")) [Var "x"; Var "y"] (fun x -> String.concat ", " (List.map pretty_expr x)) pretty_parse_error;
  should_be_ok "term" (parse term (lex_string "x * y")) (Mul (Var "x", Var "y")) pretty_expr pretty_parse_error;

  should_parse "nested operators" "1 + 2 * 3 + 4" (Add (Var "1", Add (Mul (Var "2", Var "3"), Var "4")));
