module PrimOpNameSet = Set.Make(String)

let primops = PrimOpNameSet.of_list [
  "head";
  "tail";
  "cons";
  "require";
  "lines";
  "replace";
  "regexpReplace";
  "writeFile";
  "parseInt";
  "parseNum";
  "readLine";
]

let is_primop name = PrimOpNameSet.mem name primops

