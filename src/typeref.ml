type level = int

let initial_top_level = 0
let next_level lvl = lvl + 1
let pretty_level = string_of_int
let generalizable_level ~ambient level = level >= ambient
let escaping_level ~ambient level = level > ambient
let unifiable_level ~type_level ~unif_level = unif_level >= type_level

type 'a state =
  | Unbound of level
  | Bound of 'a

type 'a t = Unique.t * 'a state ref

let make level = (Unique.fresh (), ref (Unbound level))
let get (_, ref) = !ref
let set (_, ref) value = ref := Bound value

let adjust_level max_level = function
  | _, { contents = Bound _ } -> ()
  | unique, ({ contents = Unbound level } as ref) ->
      ref := Unbound (min max_level level)

let get_unique (unique, _) = unique

let equal typeref1 typeref2 =
  Unique.equal (get_unique typeref1) (get_unique typeref2)
