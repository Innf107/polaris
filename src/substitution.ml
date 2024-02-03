open Syntax
open Syntax.Typed

type t = ty NameMap.t

let of_map = Fun.id
let apply subst name = NameMap.find_opt name subst

let apply_traversal subst =
  object
    inherit [unit] Traversal.traversal

    method! ty () =
      function
      | TyVar name as ty -> begin
          match apply subst name with
          | None -> (ty, ())
          | Some ty -> (ty, ())
        end
      | ty -> (ty, ())
  end

let apply_type subst ty = fst ((apply_traversal subst)#traverse_type () ty)
