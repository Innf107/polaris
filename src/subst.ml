open Syntax.Renamed

module UniqueMap = Map.Make(Unique)

type t = ty UniqueMap.t ref

let make () = ref UniqueMap.empty

let find unique subst =
  UniqueMap.find_opt unique !subst

(* TODO: Put this in some kind of common type operation module *)
let replace_unif : Unique.t -> ty -> ty -> ty =
  fun var replacement -> Ty.transform begin function
    | Unif (typeref, _) when Unique.equal (Typeref.get_unique typeref) var -> replacement
    | ty -> ty
    end


let add unif ty subst =
  subst := UniqueMap.add unif ty (UniqueMap.map (replace_unif unif ty) !subst)

let apply subst = Ty.transform begin function
| Unif (typeref, _) as ty ->
  begin match find (Typeref.get_unique typeref) subst with
  | None -> ty
  | Some ty' -> ty'
  end
| ty -> ty
end
