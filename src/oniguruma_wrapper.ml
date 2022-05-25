open Util
open Oniguruma

(* TODO: No idea how to return multiple matches... *)
let search : ?comptime_options : Options.compile_time Options.t 
          -> ?searchtime_options : Options.search_time Options.t
          -> string 
          -> string 
          -> string list = 
  fun ?(comptime_options = Options.none) ?(searchtime_options = Options.none) pattern haystack ->

  let regexp = match Oniguruma.create pattern comptime_options Encoding.utf8 Syntax.perl with
    | Ok(regexp) -> regexp
    | Error(msg) -> raise (Panic ("Unable to create regexp: " ^ msg ^ "\n    Pattern: '" ^ pattern ^ "'"))
  in

  match search regexp haystack 0 (String.length haystack) searchtime_options with
  | Some(region) -> 
    List.init (Region.length region) begin fun i ->
      let start = Region.capture_beg region i in
      String.sub haystack start (Region.capture_end region i - start)
    end
  | None -> []
