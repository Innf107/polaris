
module Make(Key: Map.OrderedType) = struct
  module M = Map.Make(Key)
  
  type 'a t = 'a list M.t
  type key = Key.t

  let empty = M.empty

  let add k v m =
    M.update k (function None -> Some [v] | Some vs -> Some (v :: vs)) m

  let add_list list m =
    List.fold_right (fun (k, v) m -> add k v m) list m

  let to_seq m = 
    Seq.concat_map 
      (fun (k, vs) -> Seq.map (fun v -> (k, v)) (List.to_seq vs)) 
      (M.to_seq m)

  let of_list list =
    add_list list empty

  let of_seq seq =
    add_list (List.of_seq seq) empty

  let equal comp m1 m2 = M.equal (List.equal comp) m1 m2

  let find key m = Option.value ~default:[] (M.find_opt key m)

  let union m1 m2 = M.union (fun _ l1 l2 -> Some (l1 @ l2)) m1 m2

  let update key f =
    M.update key begin fun vals -> 
      match f (Option.value ~default:[] vals) with
      | [] -> None
      | vs -> Some vs
    end
end