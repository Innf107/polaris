type t = {
  file : string;
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
}

type position = {
  line : int;
  column : int;
}
let compare_position = compare

let start loc = { line = loc.start_line; column = loc.start_col }
let end_ loc = { line = loc.end_line; column = loc.end_col }

let pretty (loc : t) =
  Printf.sprintf "%s:%d:%d-%d:%d" loc.file loc.start_line loc.start_col
    loc.end_line loc.end_col

let pretty_start (loc : t) =
  Printf.sprintf "%s:%d:%d" loc.file loc.start_line loc.start_col

let from_pos (start_pos : Lexing.position) (end_pos : Lexing.position) : t =
  {
    file = start_pos.pos_fname;
    start_line =
      start_pos.pos_lnum
      (* pos_cnum is the offset between the beginning of the buffer and the position
         and pos_bol is the offset between the beginning of the buffer and the beginning of the current line
      *);
    start_col = start_pos.pos_cnum - start_pos.pos_bol + 1;
    end_line = end_pos.pos_lnum;
    end_col = end_pos.pos_cnum - end_pos.pos_bol + 1;
  }

let internal : t =
  {
    file = "<internal>";
    start_line = 0;
    end_line = 0;
    start_col = 0;
    end_col = 0;
  }

let is_internal loc = loc = internal

let merge loc1 loc2 =
  if loc1 = internal || loc1.file = "" then loc2
  else if loc2 = internal || loc2.file = "" then loc1
  else if loc1.file = loc2.file then
    { loc1 with end_line = loc2.end_line; end_col = loc2.end_col }
  else
    raise
      (Failure
         ("Athena.Loc.merge: Trying to merge locations from different files. \
           Locations: " ^ pretty loc1 ^ " and " ^ pretty loc2))

let rec concat = function
  | [] -> internal
  | [ loc ] -> loc
  | loc :: locs -> merge loc (concat locs)

let rec concat_split = function
  | [] -> ([], internal)
  | [ (x, loc) ] -> ([ x ], loc)
  | (x, loc) :: locs ->
      let xs, loc' = concat_split locs in
      (x :: xs, merge loc loc')
