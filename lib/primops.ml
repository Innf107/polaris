module PrimOpNameSet = Set.Make(String)

let primops = PrimOpNameSet.of_list [
  "head";
  "tail";
  "cons";
  "require";
  "lines";
  "replace";
  "regexp_replace";
  "write_file";
]

let is_primop name = PrimOpNameSet.mem name primops

