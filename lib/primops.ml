module PrimOpNameSet = Set.Make(String)

let primops = PrimOpNameSet.of_list [
  "head";
  "tail";
  "require";
  "lines";
]

let is_primop name = PrimOpNameSet.mem name primops

