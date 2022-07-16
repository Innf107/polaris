open Loc
type loc = Loc.t

module Stream = Stream
module Loc = Loc

include Loc

module Make(Token : sig
  type t

  val to_string : t -> string

  val equal : t -> t -> bool
end) = struct
  type parse_error = RemainingTokens of (Token.t * loc) Stream.t
                   | UnexpectedEOF
                   | ParseError of string
                   | ParseErrorOn of string * Token.t * loc
                   | UnexpectedToken of Token.t * loc

  type 'a parser_impl = ('a * (Token.t * loc) Stream.t, parse_error) result
  type parser_arg = (Token.t * loc) Stream.t
  type 'a parser = parser_arg -> 'a parser_impl

  let parse (parser : 'a parser) stream =
    match parser stream with
    | Error err -> Error err
    | Ok(x, stream') ->
      match Stream.next stream' with
      | None -> Ok(x)
      | _ -> Error (RemainingTokens stream')
    

  let map f parser = 
        fun stream -> match parser stream with
          | Error err -> Error err
          | Ok (res, str') -> Ok(f res, str') 
      
  let pure x =
    fun stream -> Ok (x, stream)
  

  let (<*>) f_parser arg_parser =
      fun stream -> 
        match f_parser stream with
        | Error err -> Error err
        | Ok (f, stream) -> 
          match arg_parser stream with
          | Error err -> Error err
          | Ok(arg, stream) -> Ok(f arg, stream)


  let (<$>) = map

  let (<$$>) f = map (f {file="TODO"; start_col=0; start_line=0; end_col=0; end_line=0})

  let ( *>) parser1 parser2 =
    (fun x y -> y) <$> parser1 <*> parser2

  let (<* ) parser1 parser2 =
    (fun x y -> x) <$> parser1 <*> parser2

  let prod parser1 parser2 = 
    (fun x y -> (x, y)) 
    <$> parser1 
    <*> parser2

  let bind parser cont =
    fun stream ->
      match parser stream with
      | Error err -> Error err
      | Ok (res, str') -> cont res str'


  let (>>=) = bind

  let (let*) = bind

  let (<|>) left right =
    fun stream ->
      match left stream with
      | Ok res -> Ok res
      | Error _ -> right stream (* TODO: Include the error somehow *)


  let fail msg =
    fun _ -> Error (ParseError msg)
  

  let fail_error err =
    fun _ -> Error err
  

  let (<?>) p msg = 
    fun stream -> match p stream with
      | Ok res -> Ok res
      | Error (ParseErrorOn (_, t, loc) | UnexpectedToken (t, loc)) -> Error (ParseErrorOn (msg, t, loc))
      | Error _ -> Error (ParseError msg)
  
  let (<??>) msg p = p <?> msg

  let any =
    fun stream ->
      match Stream.next stream with
      | None -> Error UnexpectedEOF
      | Some (tok, stream') -> Ok(tok, stream')
  

  let satisfy_loc (f : loc -> Token.t -> bool) : (Token.t * loc) parser =
    any >>= fun (tok, loc) ->
      if f loc tok then
        pure (tok, loc)
      else
        fail_error (UnexpectedToken (tok, loc))
    

  let satisfy f =
    (fun (_, loc) -> loc) <$> satisfy_loc (fun _ t -> f t)

  let token t = 
    satisfy (Token.equal t) <?> "token: Expected '" ^ Token.to_string t ^ "'"

  let token_of (f : loc -> Token.t -> 'a option) : 'a parser =
    (fun (t, loc) -> Option.get (f loc t)) <$> satisfy_loc (fun loc t -> Option.is_some (f loc t))

  let one_of (toks : Token.t list) : (Token.t * loc) parser =
    "token: Expected one of " ^ String.concat ", " (List.map (fun tok -> "\"" ^ Token.to_string tok ^ "\"") toks)
    <??>
    satisfy_loc (fun _ x -> List.mem x toks) 

  (* This stack overflow, since it has to evaluate 'many parser', to even construct the parser *)
  let rec many parser =
    (fun stream -> begin
      List.cons <$> parser <*> many parser
    end stream)
    <|> pure []

  let some parser = 
    let* x = parser in
    let* rest = many parser in
    pure (x :: rest)

  let rec sep_by1 sep parser =
    begin
      let* x = parser in
      (fun stream -> begin
        sep *> 
        let* xs = sep_by1 sep parser in
        pure (x :: xs)
      end stream) <|> pure [x]
    end

  let sep_by sep parser = 
    sep_by1 sep parser
    <|> pure []

  let rec sep_by_trailing1 sep parser =
    begin
      let* x = parser in
      
      (fun stream -> begin 
        sep *>
        let* xs = sep_by_trailing sep parser in
        pure (x :: xs)
      end stream)
      <|> pure [x]
    end

  and sep_by_trailing sep parser = sep_by_trailing1 sep parser <|> pure []

  let optional parser =
    (fun x -> Some x) <$> parser
    <|> pure None

  let chainl1 parser op_parser =
    let (let*) = bind in
    let* x = parser in
    let rec rest x =
      (fun stream -> begin 
        let* f = op_parser in
        let* y = parser in
        rest (f x y)
      end stream) <|> pure x
    in
    rest x

  let chainl parser op_parser def =
    chainl1 parser op_parser
    <|> pure def

  let left_assoc parser op_parser =
    let (let*) = bind in
    let* x = parser in
    let rec rest x = 
      (fun stream -> begin
        let* f = op_parser in
        rest (f x) 
      end stream)
      <|> pure x
    in 
    rest x
end
