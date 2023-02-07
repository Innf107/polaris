open Syntax.Renamed

module UniqueMap = Map.Make(Unique)

type t = ty UniqueMap.t ref

let make () = ref UniqueMap.empty

let find unique subst =
  UniqueMap.find_opt unique !subst

(* TODO: Put this in some kind of common type operation module *)
let replace_unif : Unique.t -> ty -> ty -> ty =
  fun var replacement -> Ty.transform begin function
    | Unif (u, _) when Unique.equal u var -> replacement
    | ty -> ty
    end


let add unif ty subst =
  subst := UniqueMap.add unif ty (UniqueMap.map (replace_unif unif ty) !subst)

let apply subst = Ty.transform begin function
| Unif (u, _) as ty ->
  begin match find u subst with
  | None -> ty
  | Some ty' -> ty'
  end
| ty -> ty
end
