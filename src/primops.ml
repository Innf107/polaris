open Util
open Syntax
open Syntax.Renamed
module PrimOpMap = Map.Make (String)

let forall (cont : ty -> ty) =
  let a = Name.fresh "a" in
  Forall (a, cont (TyVar a))

let forall' (cont : name -> ty) =
  let a = Name.fresh "a" in
  Forall (a, cont a)

let primops =
  PrimOpMap.of_seq
    (List.to_seq
       [
         ("print", forall (fun a -> [ a ] --> Ty.unit));
         ("lines", [ String ] --> List String);
         ("split", [ String; String ] --> List String);
         ("chars", [ String ] --> List String);
         ("replace", [ String; String; String ] --> String);
         ("regexpReplace", [ String; String; String ] --> String);
         ("regexpMatch", [ String; String ] --> List String);
         ("regexpMatchGroups", [ String; String ] --> List (List String));
         ( "regexpTransform",
           [ String; [ String ] --> String; String ] --> String );
         ( "regexpTransformAll",
           [ String; [ List String ] --> String; String ] --> String );
         ("writeFile", [ String; String ] --> Ty.unit);
         ("parseInt", [ String ] --> Number);
         ("parseNum", [ String ] --> Number);
         ( "readLine",
           forall' (fun a ->
               [ String ]
               --> VariantVar ([| ("Nothing", []); ("Just", [ String ]) |], a))
         );
         ("chdir", [ String ] --> Ty.unit);
         ("exit", forall (fun a -> [ Number ] --> a));
         ("toString", forall (fun a -> [ a ] --> String));
         ("getArgv", [] --> List String);
         ( "getEnv",
           forall' (fun a ->
               [ String ]
               --> VariantVar ([| ("Nothing", []); ("Just", [ String ]) |], a))
         );
         ("fail", forall (fun a -> [ String ] --> a));
         ("scriptLocal", [ String ] --> String);
         ("commandExists", [ String ] --> Bool);
         ("ensure", [ String ] --> Ty.unit);
         ("status", [] --> Number);
         ("mod", [ Number; Number ] --> Number);
         ("floor", [ Number ] --> Number);
         ("exceptionMessage", [ Exception ] --> String);
       ])

let is_primop name = PrimOpMap.mem name primops

let command_failure_exception =
  Name.{ name = "CommandFailure"; index = Name.primop_index }

let program_args_too_large_exception =
  Name.{ name = "ProgramArgumentsTooLarge"; index = Name.primop_index }

let executable_not_found_exception =
  Name.{ name = "ExecutableNotFound"; index = Name.primop_index }

let prim_exceptions =
  PrimOpMap.of_seq
    (List.to_seq
       [
         ( "CommandFailure",
           ( command_failure_exception,
             [
               RecordClosed
                 [|
                   ("program", String);
                   ("arguments", List String);
                   ("exitCode", Number);
                   ("stdout", String);
                 |];
             ] ) );
         ("ProgramArgumentsTooLarge", (program_args_too_large_exception, []));
         ("ExecutableNotFound", (executable_not_found_exception, [ String ]));
       ])
