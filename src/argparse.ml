open Util

type flag_info = {
  aliases: string list
; arg_count: int
; action: string list -> unit
}

module RuntimeError = struct
  exception InvalidFlag of string
  exception NotEnoughArguments of string * int
end

let run : flag_info list -> string list -> string list =
  fun flags args ->
    (* TODO: intern this into a map *)
    let get_flag flag = 
      match List.find_opt (fun info -> List.exists (fun x -> x == flag) info.aliases) flags with
      | Some(x) -> x
      | None -> raise (RuntimeError.InvalidFlag flag)
    in
    
    let rec go = function
    | (arg::args) when String.starts_with ~prefix:"--" arg ->
      let info = get_flag arg in
      begin match Util.split_at_exact info.arg_count args with
      | None -> raise (RuntimeError.NotEnoughArguments (arg, info.arg_count))
      | Some(flag_args, remaining) -> 
        info.action flag_args;
        go remaining
      end

    | (arg::args) when String.starts_with ~prefix:"-" arg -> 
      raise TODO

    | (arg::args) ->
      arg :: go args
    | [] -> []
    in
    go args

