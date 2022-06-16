type loc = {    
    file : string;
    start_line : int;
    start_col : int;
    end_line : int;
    end_col : int
}  

module Stream = Stream

module Make(Token : sig
  type t
  val to_string : t -> string

  val equal : t -> t -> bool
end) = struct
  type parse_error = RemainingTokens of Token.t Stream.t
                   | UnexpectedEOF
                   | ParseError of string
                   | ParseErrorOn of string * Token.t
                   | UnexpectedToken of Token.t

  type 'a parser = {
    run : Token.t Stream.t -> ('a * Token.t Stream.t, parse_error) result
  }

  let parse parser stream =
    match parser.run stream with
    | Error err -> Error err
    | Ok(x, stream') ->
      match Stream.next stream' with
      | None -> Ok(x)
      | _ -> Error (RemainingTokens stream')
    

  let map f parser = {
        run = fun stream -> match parser.run stream with
          | Error err -> Error err
          | Ok (res, str') -> Ok(f res, str') 
      }

  let pure x = {
    run = fun stream -> Ok (x, stream)
  }

  let (<*>) f_parser arg_parser = {
      run = fun stream -> 
        match f_parser.run stream with
        | Error err -> Error err
        | Ok (f, stream) -> 
          match arg_parser.run stream with
          | Error err -> Error err
          | Ok(arg, stream) -> Ok(f arg, stream)
  }

  let (<$>) = map

  let ( *>) parser1 parser2 =
    (fun x y -> y) <$> parser1 <*> parser2

  let (<* ) parser1 parser2 =
    (fun x y -> x) <$> parser1 <*> parser2

  let prod parser1 parser2 = 
    pure (fun x y -> (x, y)) 
    <*> parser1 
    <*> parser2

  let bind parser cont = {
    run = fun stream ->
      match parser.run stream with
      | Error err -> Error err
      | Ok (res, str') -> (cont res).run str'
  }

  let (>>=) = bind

  let (let*) = bind

  let (<|>) left right = {
    run = fun stream ->
      match left.run stream with
      | Ok res -> Ok res
      | Error _ -> right.run stream (* TODO: Include the error somehow *)
  }

  let fail msg = {
    run = fun _ -> Error (ParseError msg)
  }

  let fail_error err = {
    run = fun _ -> Error err
  }

  let (<?>) p msg = {
    run = fun stream -> match p.run stream with
      | Ok res -> Ok res
      | Error (ParseErrorOn (_, t) | UnexpectedToken t) -> Error (ParseErrorOn (msg, t))
      | Error _ -> Error (ParseError msg)
  }
  let (<??>) msg p = p <?> msg

  let any = {
    run = fun stream ->
      match Stream.next stream with
      | None -> Error UnexpectedEOF
      | Some (tok, stream') -> Ok(tok, stream')
  }

  let satisfy f =
    any >>= fun tok ->
      if f tok then
        pure tok
      else
        fail_error (UnexpectedToken tok)

  let token t = 
    satisfy (Token.equal t) <?> "token: Expected '" ^ Token.to_string t ^ "'"

  let token_of f =
    (fun t -> Option.get (f t)) <$> satisfy (fun t -> Option.is_some (f t))

  let one_of toks =
    "token: Expected one of " ^ String.concat ", " (List.map (fun tok -> "\"" ^ Token.to_string tok ^ "\"") toks)
    <??>
    satisfy (fun x -> List.mem x toks) 

  (* This stack overflow, since it has to evaluate 'many parser', to even construct the parser *)
  let rec many parser =
    begin
      List.cons <$> parser <*> many parser
    end
    <|> pure []

  let some parser = 
    let* x = parser in
    let* rest = many parser in
    pure (x :: rest)

  let rec sep_by_trailing sep parser =
    begin
      let* x = parser in
      
      begin 
        sep *>
        let* xs = sep_by_trailing sep parser in
        pure (x :: xs)
      end
      <|> pure [x]
    end
    <|> pure []

  let optional parser =
    (fun x -> Some x) <$> parser
    <|> pure None
end
