type binding = Unique.t

module Binding = struct
  type t = binding
  let compare = Unique.compare
end

type t =
  | Dictionary of binding
  | Meta of meta
  | Apply of t * t list

and meta = t option ref * Unique.t

let make_binding = Unique.fresh
let make_empty_meta () = (ref None, Unique.fresh ())

let rec follow_metas : t -> t =
 fun evidence ->
  match evidence with
  | Meta (({ contents = Some evidence } as meta_ref), _) ->
      (* we do path compression so that repeated traversals don't need to re-follow meta variables here *)
      let followed_evidence = follow_metas evidence in
      meta_ref := Some followed_evidence;
      followed_evidence
  | evidence -> evidence

let rec pretty = function
  | Dictionary binding -> pretty_binding binding
  | Meta meta -> pretty_meta meta
  | Apply (entailed_evidence, arguments) ->
      pretty entailed_evidence ^ "("
      ^ String.concat ", " (List.map pretty arguments)
      ^ ")"

and pretty_meta = function
  | { contents = None }, unique -> "?" ^ Unique.display unique
  | { contents = Some evidence }, _ -> pretty evidence

and pretty_binding binding = "$" ^ Unique.display binding

let set_meta (meta_ref, unique) evidence =
  match !meta_ref with
  | Some previous ->
      Util.panic __LOC__
        ("Trying to set already bound evidence meta variable. previous value: "
       ^ pretty previous ^ ". new value: " ^ pretty evidence)
  | None -> meta_ref := Some evidence
