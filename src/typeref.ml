
type 'a t = Unique.t * 'a option ref

let make () = (Unique.fresh (), ref None)

let get (_, ref) = !ref

let set (_, ref) value = ref := Some value

let get_unique (unique, _) = unique

let equal typeref1 typeref2 = Unique.equal (get_unique typeref1) (get_unique typeref2)
