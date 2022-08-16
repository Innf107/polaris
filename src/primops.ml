module PrimOpNameSet = Set.Make(String)

let primops = PrimOpNameSet.of_list [
  "print";
  "head";
  "tail";
  "cons";
  "require";
  "lines";
  "split";
  "replace";
  "regexpReplace";
  "regexpMatch";
  "regexpTransform";
  "regexpTransformAll";
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
  "commandExists";
  "ensure";
  "status";
]

let is_primop name = PrimOpNameSet.mem name primops

