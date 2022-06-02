open Util

type flag_info = {
  aliases: string list
; arg_count: int
; action: string list -> unit
}

(* TODO *)
let with_usage description flags msg = msg

let run : flag_info list -> string -> (string -> void) -> string list -> string list =
  fun flags description fail_fun args ->
    let fail_fun msg = absurd (fail_fun (with_usage description flags msg)) in
    (* TODO: intern this into a map *)
    let get_flag flag = 
      match List.find_opt (fun info -> List.exists (fun x -> String.equal x flag) info.aliases) flags with
      | Some(x) -> x
      | None -> fail_fun ("Invalid flag: '" ^ flag ^ "'")
    in
    
    let rec go = function
    | (arg::args) when String.starts_with ~prefix:"--" arg ->
      let info = get_flag arg in
      begin match Util.split_at_exact info.arg_count args with
      | None -> fail_fun ("Not enough arguments for flag '" ^ arg ^ "'. Expected " ^ Int.to_string info.arg_count ^ " arguments.")
      | Some(flag_args, remaining) -> 
        info.action flag_args;
        go remaining
      end

    | (arg::args) when String.starts_with ~prefix:"-" arg -> 
      raise (Panic "Short flags NYI")

    | (arg::args) ->
      arg :: go args
    | [] -> []
    in
    go args

