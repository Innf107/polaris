type t = int

let unique_counter = ref 0

let fresh () =
  let value = !unique_counter in
  unique_counter := !unique_counter + 1;
  value

let equal = Int.equal
let compare = Int.compare
let display = Int.to_string
