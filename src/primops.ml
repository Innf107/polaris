module PrimOpNameSet = Set.Make(String)

let primops = PrimOpNameSet.of_list [
  "head";
  "tail";
  "cons";
  "require";
  "lines";
  "replace";
  "regexpReplace";
  "regexpMatch";
  "writeFile";
  "parseInt";
  "parseNum";
  "readLine";
  "readLineDefault";
  "chdir";
  "exit";
  "toString";
  "getArgv";
  "getEnv";
  "insert";
  "fail";
]

let is_primop name = PrimOpNameSet.mem name primops

