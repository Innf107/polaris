open Util

type program_info = {
  description : string;
  usage : string;
  name : string;
}

type flag_info = {
  aliases : string list;
  arg_count : int;
  action : string list -> unit;
  description : string;
  required : bool;
}

let pretty_flags flags =
  let flag_data =
    List.map
      (fun flag -> (String.concat ", " flag.aliases, flag.description))
      flags
  in
  let max_flag_width =
    Util.max 0 (List.map (fun (x, _) -> String.length x) flag_data)
  in
  String.concat "\n    "
    ("    "
    :: List.map
         (fun (flags, descr) ->
           Util.pad_right (max_flag_width + 1) ' ' flags ^ " " ^ descr)
         flag_data)

(* TODO *)
let with_usage prog_info flags msg =
  msg ^ "\n\n" ^ "Usage: " ^ prog_info.name ^ " " ^ prog_info.usage ^ "\n"
  ^ (if prog_info.description = "" then "" else prog_info.description ^ "\n")
  ^ "\n" ^ "Options" ^ pretty_flags flags

let run :
    flag_info list ->
    program_info ->
    (string -> void) ->
    string list ->
    string list =
 fun flags prog_info fail_fun args ->
  let fail_fun msg = absurd (fail_fun (with_usage prog_info flags msg)) in
  let required_flags =
    List.filter_map
      (fun def -> if def.required then Some def.aliases else None)
      flags
  in

  (* TODO: intern this into a map *)
  let get_flag flag =
    match
      List.find_opt
        (fun info -> List.exists (fun x -> String.equal x flag) info.aliases)
        flags
    with
    | Some x -> x
    | None -> fail_fun ("Invalid flag: '" ^ flag ^ "'")
  in

  let rec go required_flags =
    let process_flag flag args =
      let info = get_flag flag in
      begin
        match Util.split_at_exact info.arg_count args with
        | None ->
            fail_fun
              ("Not enough arguments for flag '" ^ flag ^ "'. Expected "
              ^ Int.to_string info.arg_count
              ^ " arguments.")
        | Some (flag_args, remaining) ->
            let required_flags =
              match
                Util.extract
                  (fun aliases -> List.mem flag aliases)
                  required_flags
              with
              | None -> required_flags
              | Some (_, rest) -> rest
            in
            info.action flag_args;
            go required_flags remaining
      end
    in
    function
    | arg :: args when String.starts_with ~prefix:"--" arg ->
        process_flag arg args
    | "-" :: args -> "-" :: go required_flags args
    | arg :: args when String.starts_with ~prefix:"-" arg ->
        let first_flag = "-" ^ String.sub arg 1 1 in
        let remaining = "-" ^ String.sub arg 2 (String.length arg - 2) in

        begin
          match remaining with
          | "-" -> process_flag first_flag args
          | _ -> process_flag first_flag (remaining :: args)
        end
    | arg :: args -> arg :: go required_flags args
    | [] -> (
        match required_flags with
        | [] -> []
        | _ ->
            let pretty_flag = function
              | [ x ] -> x
              | xs -> "(" ^ String.concat " | " xs ^ ")"
            in
            fail_fun
              ("Missing required flags: "
              ^ String.concat ", " (List.map pretty_flag required_flags)))
  in
  go required_flags args
