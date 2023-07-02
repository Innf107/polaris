open Loc

type text_style = {
  custom : color:string -> string -> string;

  bold : string -> string;

  identifier : string -> string;
  number : int -> string;
  ty : string -> string;
  ty_secondary : string -> string;

  emphasis : string -> string;

  error : string -> string;
  warning : string -> string;

  note : string -> string
}
   
let make_text_style ~enable_color =
  if enable_color then {
    custom = (fun ~color message -> color ^ message ^ "\x1b[0m");

    bold = (fun message -> "\x1b[1m" ^ message ^ "\x1b[0m");

    identifier = (fun message -> "\x1b[1m" ^ message ^ "\x1b[0m");
    number = (fun number -> "\x1b[1m\x1b[93m" ^ string_of_int number ^ "\x1b[0m");

    ty = (fun message -> "\x1b[1m\x1b[96m" ^ message ^ "\x1b[0m");
    ty_secondary = (fun message -> "\x1b[1m\x1b[94m" ^ message ^ "\x1b[0m");

    emphasis = (fun message -> "\x1b[1m" ^ message ^ "\x1b[0m");

    (* Not all terminals support 24-bit colors, so we first print the regular 8-bit escape
       sequence for red and then override that with our fancy 24-bit full red, if available. *)
    error = (fun message -> "\x1b[20m\x1b[31m\x1b[38;2;255;0;0m" ^ message ^ "\x1b[0m");

    warning = (fun message -> "\x1b[35m\x1b[38;2;192;0;192m" ^ message ^ "\x1b[0m");

    note = (fun message -> "\x1b[38;5;8m" ^ message ^ "\x1b[0m");
  } else {
    custom = (fun ~color message -> message);
    bold = Fun.id;

    identifier = (fun message -> "'" ^ message ^ "'");
    number = string_of_int;

    ty = (fun message -> "'" ^ message ^ "'");
    ty_secondary = (fun message -> "'" ^ message ^ "'");

    emphasis = Fun.id;

    error = Fun.id;
    warning = Fun.id;

    note = Fun.id;
  }
   


(* Extract the line at the (1-based!) index, as well as the ones immediately preceeding and succeeding it *)
let extract_line =
  fun index in_channel -> 
    (* Skip all lines preceeding the index. *)
    for _=1 to index-1 do
      let _ = In_channel.input_line in_channel in 
      ()
    done;
    Option.value ~default:"" (In_channel.input_line in_channel)

let extract_source_fragment loc in_channel text_style error_or_warning =
  let line = extract_line loc.start_line in_channel in

  let color = match error_or_warning with
  | `Error -> text_style.error
  | `Warning -> text_style.warning
  in
  let line_before = String.sub line 0 (loc.start_col - 1) in
  let line_at_error, line_after = 
    if loc.end_line > loc.start_line then
      (* The error continues on the next line so we highlight the entirity of this one *)
      ( String.sub line (loc.start_col - 1) (String.length line - (loc.start_col - 1))
      , ""
      )
    else
      ( String.sub line (loc.start_col - 1) (loc.end_col - (loc.start_col))
      , String.sub line (loc.end_col - 1) (String.length line - (loc.end_col - 1))
      )
  in
  let underline_string = color (String.make (String.length line_at_error) '^') in
  let underline = String.make (String.length line_before) ' ' ^ text_style.error underline_string
  in
  ( ""
  , line_before ^ color line_at_error ^ line_after
  , underline
  )


