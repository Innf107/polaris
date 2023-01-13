open Loc

(* Extract the line at the (1-based!) index, as well as the ones immediately preceeding and succeeding it *)
let extract_line : int -> In_channel.t -> (string * string * string) =
  fun index in_channel -> 
    (* Skip all lines preceeding the index. *)
    for _=2 to index-1 do
      let _ = In_channel.input_line in_channel in 
      ()
    done;

    let first_line = 
      if index <= 1 then
        ""
      else
        Option.value ~default:"" (In_channel.input_line in_channel)
    in
    let second_line = Option.value ~default:"" (In_channel.input_line in_channel) in
    let third_line = Option.value ~default:"" (In_channel.input_line in_channel) in
    (first_line, second_line, third_line)

let extract_source_fragment loc in_channel style_function =
  let preceeding_line, line, succeeding_line = extract_line loc.start_line in_channel in


  let line_before = String.sub line 0 (loc.start_col - 1) in
  let line_at_error, line_after = 
    if loc.end_line > loc.start_line then
      (* The error continues on the next line so we highlight the entirity of this one *)
      ( String.sub line (loc.start_col - 1) (String.length line - loc.start_col)
      , ""
      )
    else
      ( String.sub line (loc.start_col - 1) (loc.end_col - (loc.start_col - 1))
      , String.sub line (loc.end_col - 1) (String.length line - (loc.end_col - 1))
      )
  in
  let succeeding_at_error, succeeding_after =
    match loc.end_line - loc.start_line with
    | 0 ->
      (* The error is contained to the previous line and doesn't continue on this one *)
      ("", succeeding_line)
    | 1 ->
      (* The error partially continues on this line but does not continue even further *)
      ( String.sub succeeding_line 0 (loc.end_col - 1)
      , String.sub succeeding_line (loc.end_col - 1) (String.length succeeding_line - (loc.end_col - 1))
      )
    | _ ->
      (* The error fully contains this line and continues on the next one *)
      (succeeding_line, "")
  in

  ( preceeding_line
  , line_before ^ style_function line_at_error ^ line_after
  , style_function succeeding_at_error ^ succeeding_after
  )
