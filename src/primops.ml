open Util
open Syntax
open Syntax.Renamed

module PrimOpMap = Map.Make(String)

let forall (cont : ty -> ty) = let a = Name.fresh "a" in Forall (a, cont (Var a))  

let primops = PrimOpMap.of_seq (List.to_seq [
  "print", forall (fun a -> [a] --> Tuple [||]);
  "head", forall (fun a -> [List a] -->a);
  "tail", forall (fun a -> [List a] --> List a);
  "cons", forall (fun a -> [a; List a] --> List a);
  "require", Tuple [||]; (* TODO: require doesn't work with static types *)
  "lines", [String] --> List String;
  "split", [String; String] --> List String;
  "replace", [String; String; String] --> String;
  "regexpReplace", [String; String; String] --> String;
  "regexpMatch", [String; String] --> String;
  "regexpTransform", [String; [String] --> String] --> String;
  "regexpTransformAll", [String; [String] --> String] --> String;
  "writeFile", [String; String] --> Tuple [||];
  "parseInt", [String] --> Number;
  "parseNum", [String] --> Number;
  "readLine", [String] --> Number;
  "readLineDefault", [String; String] --> Number;
  "chdir", [String] --> Tuple [||];
  "exit", forall (fun a -> [Number] --> a);
  "toString", forall (fun a -> [a] --> String);
  "getArgv", [] --> List String;
  "getEnv", [] --> List String;
  "insert", Tuple [||]; (* TODO: Map types are NYI*)
  "mapToList", Tuple [||]; (* TODO: Map types are NYI*)
  "fail", forall (fun a -> [String] --> a);
  "scriptLocal", [String] --> String;
  "commandExists", [String] --> String;
  "ensure", [String] --> String;
  "status", [] --> Number;
])

let is_primop name = PrimOpMap.mem name primops

