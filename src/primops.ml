module PrimOpNameSet = Set.Make(String)

let primops = PrimOpNameSet.of_list [
  "print";
  "head";
  "tail";
  "cons";
  "require";
  "lines";
  "replace";
  "regexpReplace";
  "regexpMatch";
  "regexpTransform";
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
  "mapToList";
  "fail";
  "scriptLocal";
]

let is_primop name = PrimOpNameSet.mem name primops

