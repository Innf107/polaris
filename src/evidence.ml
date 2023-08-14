type source =
    | Dictionary of Unique.t

type target =
    | BoundDictionary of source option ref

let display_source (Dictionary unique) = Unique.display unique
let display_target = function
  | BoundDictionary { contents = None } -> "_"
  | BoundDictionary { contents = Some source } -> display_source source
  
let make_source () = Dictionary (Unique.fresh ())
let make_empty_target () = BoundDictionary (ref None)

let fill_target (BoundDictionary target_ref as target) source =
  match !target_ref with
  | Some _ ->
      Util.panic __LOC__
        ("Trying to fill already filled evidence target ("
       ^ display_target target ^ ") with source (" ^ display_source source ^ ")"
        )
  | None -> target_ref := Some source
