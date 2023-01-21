open Loc

type color = Red | Purple | Custom of string

let make_coloring ~enable_color color message =
  if not enable_color then
    message
  else
    match color with
    (* Not all terminals support 24-bit colors, so we first print the regular 8-bit escape
       sequence for red and then override that with our fancy 24-bit full red, if available. *)
    | Red -> "\x1b[31m\x1b[38;2;255;0;0m" ^ message ^ "\x1b[0m"
    | Purple -> "\x1b[35m\x1b[38;2;192;0;192m" ^ message ^ "\x1b[0m"
    | Custom prefix -> prefix ^ message ^ "\x1b[0m"

type text_style = {
    color : color -> string -> string;
    bold : string -> string;

    identifier : string -> string;
    number : int -> string;
    ty : string -> string;
    ty_secondary : string -> string;
  }
   
let make_text_style ~enable_color =
  if enable_color then {
    color = make_coloring ~enable_color;
    bold = (fun message -> "\x1b[1m" ^ message ^ "\x1b[0m");
    
    identifier = (fun message -> "\x1b[1m" ^ message ^ "\x1b[0m");
    number = (fun number -> "\x1b[1m\x1b[93m" ^ string_of_int number ^ "\x1b[0m");

    ty = (fun message -> "\x1b[1m\x1b[96m" ^ message ^ "\x1b[0m");
    ty_secondary = (fun message -> "\x1b[1m\x1b[94m" ^ message ^ "\x1b[0m");
  } else {
    color = make_coloring ~enable_color;
    bold = Fun.id;

    identifier = (fun message -> "'" ^ message ^ "'");
    number = string_of_int;

    ty = (fun message -> "'" ^ message ^ "'");
    ty_secondary = (fun message -> "'" ^ message ^ "'");
  }
   

module Style = struct
  type t = {
    apply : bool -> string -> string;
    underline_style : (char * color) option;
    enable_color : bool
  }

  let plain = {
    apply = (fun _ message -> message);
    underline_style = None;
    enable_color = true;
  }

  let with_color color style = 
    { style with apply = fun enable_color message ->
      make_coloring ~enable_color color (style.apply enable_color message)
    }

  let with_bold style =
    { style with apply = fun enable_color message ->
        if enable_color then
          "\x1b[1m" ^ (style.apply enable_color message) ^ "\x1b[0m"
        else
          style.apply enable_color message
    }

  let with_underline underline_char underline_color style =
    { style with underline_style = Some (underline_char, underline_color) }

  let with_raw_transform transformation style =
    { style with apply = fun enable_color message ->
      transformation (style.apply enable_color message)
    }

  let disable_color style =
    { style with enable_color = false }

  let is_color_enabled style = style.enable_color


end


(* Extract the line at the (1-based!) index, as well as the ones immediately preceeding and succeeding it *)
let extract_line =
  fun index in_channel -> 
    (* Skip all lines preceeding the index. *)
    for _=1 to index-1 do
      let _ = In_channel.input_line in_channel in 
      ()
    done;
    Option.value ~default:"" (In_channel.input_line in_channel)

let extract_source_fragment loc in_channel (style : Style.t) =
  let apply_color = make_coloring ~enable_color:(Style.is_color_enabled style) in

  let line = extract_line loc.start_line in_channel in

  let line_before = String.sub line 0 (loc.start_col - 1) in
  let line_at_error, line_after = 
    if loc.end_line > loc.start_line then
      (* The error continues on the next line so we highlight the entirity of this one *)
      ( String.sub line (loc.start_col - 1) (String.length line - loc.start_col)
      , ""
      )
    else
      ( String.sub line (loc.start_col - 1) (loc.end_col - (loc.start_col))
      , String.sub line (loc.end_col - 1) (String.length line - (loc.end_col - 1))
      )
  in
  let underline = match style.underline_style with
    | None -> ""
    | Some (char, color) ->
      let underline_string = String.make (String.length line_at_error) char in
        String.make (String.length line_before) ' ' 
      ^ if style.enable_color then apply_color color underline_string else underline_string
  in
  ( ""
  , line_before ^ style.apply style.enable_color line_at_error ^ line_after
  , underline
  )


