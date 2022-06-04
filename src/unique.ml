
type t = int

let counter = ref 0

let fresh () =
  let result = !counter in
  counter := result + 1;
  result

let equal = Int.equal
