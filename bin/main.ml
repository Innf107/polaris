open Polaris
open Polaris.Syntax
open Polaris.Eval  (* Required for exceptions *)
open Polaris.Lexer (* Required for exceptions *)
open Polaris.Driver

let fatal_error (message : string) = 
  print_endline "~~~~~~~~~~~~~~ERROR~~~~~~~~~~~~~~";
  print_endline message;
  print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
  exit 1

let warning (message : string) =
  print_endline "~~~~~~~~~~~~~WARNING~~~~~~~~~~~~~~";
  print_endline message;
  print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

let repl_error (message : string) = 
  print_endline ("\x1b[38;2;255;0;0m\x02ERROR\x1b[0m\x02:\n" ^ message);

type run_options = {
  interactive : bool;
  print_ast : bool;
  print_renamed : bool;
  print_tokens : bool;
}

let pretty_call_trace (locs : loc list) =
  match locs with
  | [] -> ""
  | _ -> "Call trace:"
       ^ "\n    " ^ String.concat "\n    " (List.map Loc.pretty locs)

let handle_errors print_fun f = 
  let open Rename.RenameError in 
  let open Eval.EvalError in
  try 
    f ()
  with 
  | ParseError loc -> print_fun ("Parse Error at " ^ Loc.pretty loc)
  | Sys_error msg -> print_fun ("System error: " ^ msg)
  (* RenameError *)
  | VarNotFound (x, loc) -> print_fun (Loc.pretty loc ^ ": Variable not found: '" ^ x ^ "'")
  | LetSeqInNonSeq (expr, loc) -> print_fun (
        Loc.pretty loc ^ ": Let expression without 'in' found outside a sequence expression.\n"
      ^ "    Expression: " ^ Parsed.pretty expr
      )
  (* EvalError *)
  | DynamicVarNotFound (x, loc::locs) -> print_fun (
        Loc.pretty loc ^ ": Variable not found during execution: '" ^ Name.pretty x ^ "'\n"
      ^ "This is definitely a bug in the interpreter"
      ^ pretty_call_trace locs
      )
  | NotAValueOfType(ty, value, cxt, loc :: locs) -> print_fun (
        Loc.pretty loc ^ ": Not a value of type " ^ ty ^ "."
      ^ "\n    Context: " ^ cxt
      ^ "\n      Value: " ^ Value.pretty value
      ^ "\n" ^ pretty_call_trace locs
      )  
  | TryingToApplyNonFunction (value, loc::locs) -> print_fun (
      Loc.pretty loc ^ ": Trying to apply a value that is not a function: " ^ Value.pretty value
      ^ "\n" ^ pretty_call_trace locs
    )
  | TryingToLookupInNonMap (value, key, loc::locs) -> print_fun (
      Loc.pretty loc ^ ": Trying to lookup key '" ^ key ^ "' in non-map value: " ^ Value.pretty value
      ^ "\n" ^ pretty_call_trace locs
    )
  | TryingToLookupDynamicInNonMap (value, loc::locs) -> print_fun (
      Loc.pretty loc ^ ": Trying to lookup dynamic key in non-map value: " ^ Value.pretty value
      ^ "\n" ^ pretty_call_trace locs
    )
  | InvalidKey (key, map, loc::locs) -> print_fun (
      Loc.pretty loc ^ ": Invalid key '" ^ Value.pretty key ^ "' in dynamic lookup in map: " ^ Value.pretty (MapV map)
      ^ "\n" ^ pretty_call_trace locs
    )

  | MapDoesNotContain (map, key, loc::locs) -> print_fun (
      Loc.pretty loc ^ ": Map does not contain key '" ^ key ^ "': " ^ Value.pretty (MapV map)
      ^ "\n" ^ pretty_call_trace locs
    )
  | InvalidNumberOfArguments (params, vals, loc::locs) -> print_fun (
      Loc.pretty loc ^ ": Invalid number of arguments in function call.\n"
                     ^ "Expected " ^ Int.to_string (List.length params) ^ " arguments, but received " ^ Int.to_string (List.length vals) ^ ".\n"
                     ^ "    Expected: (" ^ String.concat ", " (List.map Name.original_name params) ^ ")\n"
                     ^ "      Actual: (" ^ String.concat ", " (List.map Value.pretty vals) ^ ")"
                     ^ "\n" ^ pretty_call_trace locs
      )
  | PrimOpArgumentError (primop_name, vals, msg, loc::locs) -> print_fun (
      Loc.pretty loc ^ ": Invalid arguments to builtin function '" ^ primop_name ^ "': " ^ msg ^ "\n"
                     ^ "    Arguments: " ^ Value.pretty (ListV vals)
                     ^ "\n" ^ pretty_call_trace locs
    )
  | InvalidOperatorArgs (op_name, vals, loc::locs) -> print_fun (
      Loc.pretty loc ^ ": Invalid arguments for operator '" ^ op_name ^ "'\n"
                      ^ "    Arguments: " ^ Value.pretty (ListV vals)
                      ^ "\n" ^ pretty_call_trace locs
    )
  | NonNumberInRangeBounds (start_val, end_val, loc::locs) -> print_fun (
      Loc.pretty loc ^ ": Non-number in range bounds.\n"
                     ^ "    Range: [" ^ Value.pretty start_val ^ " .. " ^ Value.pretty end_val ^ "]"
                     ^ "\n" ^ pretty_call_trace locs
    )
  | NonBoolInListComp (value, loc::locs) -> print_fun (
    Loc.pretty loc ^ ": Non-boolean in list comprehension condition: " ^ Value.pretty value
                   ^ "\n" ^ pretty_call_trace locs
  )
  | NonListInListComp (value, loc::locs) -> print_fun (
    Loc.pretty loc ^ ": Trying to draw from a non-list in a list comprehension: " ^ Value.pretty value
                   ^ "\n" ^ pretty_call_trace locs
  )
  | InvalidProcessArg (value, loc::locs) -> print_fun (
      Loc.pretty loc ^ ": Argument cannot be passed to an external process in an !-Expression.\n"
                     ^ "    Argument: " ^ Value.pretty value
                     ^ "\n" ^ pretty_call_trace locs
    )
  | NonProgCallInPipe (expr, loc::locs) -> print_fun (
      Loc.pretty loc ^ ": Non-program call expression found in pipe: " ^ Renamed.pretty expr
      ^ "\n" ^ pretty_call_trace locs
    )
  | RuntimeError (msg, loc::locs) -> print_fun (
    Loc.pretty loc ^ ": Runtime error: " ^ msg
    ^ "\n" ^ pretty_call_trace locs
  )
  | ModuleNotFound (modName, triedPaths) -> print_fun (
      "Module not found: " ^ modName ^ "\n"
    ^ "Tried paths:"
    ^ "\n    " ^ String.concat "\n    " triedPaths
  )
  | AwaitNonPromise (value, loc::locs) -> print_fun (
    Loc.pretty loc ^ ": Trying to await non-promise: " ^ Value.pretty value
    ^ "\n" ^ pretty_call_trace locs
  )
  | NonExhaustiveMatch (value, loc::locs) -> print_fun (
    Loc.pretty loc ^ ": Non-exhaustive pattern match does not cover value: " ^ Value.pretty value
  )
  | LexError (InvalidOperator (loc, name)) -> print_fun (
    Loc.pretty loc ^ ": Invalid operator: '" ^ name ^ "'"
  )
  (* We can safely abort the program, since this can only ever happen when directly
     running a script *)
  | ArgParseError msg -> print_endline msg; exit 1

  | EnsureFailed (path, loc :: locs) -> print_fun (
    Loc.pretty loc ^ ": Required command not installed: '" ^ path ^  "'"
    ^ "\n" ^ pretty_call_trace locs
  )

let run_repl_with (scope : Rename.RenameScope.t) (env : eval_env) (options : run_options) : unit =
  Sys.catch_break true;
  let driver_options = {
      filename = "<interactive>"
      (* argv.(0) is "", to signify that this is a repl process.
         This is in line with what python does. *)
    ; argv = env.argv
    ; print_ast = options.print_ast
    ; print_renamed = options.print_renamed
    ; print_tokens = options.print_tokens
    } in
  print_endline "Welcome to Polaris! Press Ctrl+D or type \"exit(0)\" to exit.";
  let rec go env scope =
    try
      handle_errors (fun msg -> repl_error msg; go env scope)
        (fun _ -> 
          let prompt = "\x1b[38;5;160mλ>\x1b[0m " in
          match Bestline.bestline prompt with
          | None -> 
            exit 0
          | Some input -> 
            let result, new_env, new_scope = Driver.run_env driver_options (Lexing.from_string input) env scope in

            begin match result with
            | UnitV -> ()
            | _ -> print_endline (" - " ^ Value.pretty result)
            end;
            go new_env new_scope)
    with
    | Sys.Break -> 
      print_newline ();
      go env scope
  in
  go env scope

let run_file (options : run_options) (filepath : string) (args : string list) = 
  let driver_options = {
    filename = filepath
  ; argv = filepath :: args
  ; print_ast = options.print_ast
  ; print_renamed = options.print_renamed
  ; print_tokens = options.print_tokens
  } in
  handle_errors fatal_error (fun _ -> 
    let _, env, scope = 
      Driver.run_env 
        driver_options 
        (Lexing.from_channel (open_in filepath))
        (EvalInst.empty_eval_env driver_options.argv)
        Rename.RenameScope.empty
    in
    if options.interactive then
      run_repl_with scope env options 
    )
  

let run_repl options = 
  let argv = "" :: List.tl (Array.to_list Sys.argv) in
  run_repl_with Rename.RenameScope.empty (EvalInst.empty_eval_env argv) options

let usage_message = 
  "Usage: polaris [options] [file] [program options]

      --backend         Select the interpreter backend. Possible values: eval, bytecode (WIP)
      --print-ast       Print the parsed abstract syntax tree. 
                        This is only useful if you are trying to debug the interpreter
      --print-renamed   Print the renamed syntax tree.
                        This is only useful if you are trying to debug the interpreter
      --print-tokens    Print the tokens generated by the lexer without running the code.
                        This is only useful if you are trying to debug the interpreter
  "

let fail_usage : 'a. string -> 'a =
  fun msg ->
    print_endline (msg ^ "\n\n" ^ usage_message);
    exit 1

let parse_args () : run_options * string list =
  let default_options : run_options = {
      interactive = false;
      print_ast = false;
      print_renamed = false;
      print_tokens = false;
  } in
  let rec go options = function
  | ("--help" :: args) ->
    print_endline usage_message;
    exit 0
  | (("-i" | "--interactive") :: args) ->
    go ({options with interactive = true}) args
  | ("--print-ast" :: args) -> 
    go ({options with print_ast = true}) args
  | ("--print-renamed" :: args) -> 
    go ({options with print_renamed = true}) args
  | ("--print-tokens" :: args) ->
    go ({options with print_tokens = true}) args
  | (option :: args) when String.starts_with ~prefix:"-" option ->
    fail_usage ("Invalid option: '" ^ option ^ "'.")
  | args -> (options, args) 
  in
  go default_options (List.tl (Array.to_list Sys.argv))


let () =

  let options, args = parse_args () in

  match args with
  | [] -> run_repl options
  | (filepath :: args) -> ignore (run_file options filepath args)
  ;
  Promise.await_remaining ()
