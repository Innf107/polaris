open Polaris
open Polaris.Ast
open Polaris.Eval
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
  print_ast : bool;
  print_renamed : bool;
  backend : backend;
}

let pretty_call_trace (locs : loc list) =
  "Call trace:"
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
      ^ "    Expression: " ^ StringExpr.pretty expr
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
      Loc.pretty loc ^ ": Non-program call expression found in pipe: " ^ NameExpr.pretty expr
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

let run_file (options : run_options) (filepath : string) (args : string list) = 
  let _ = match options.backend with
  | EvalBackend -> ()
  | BytecodeBackend -> warning ("The bytecode backend is experimental and very incomplete. It will probably not work as expected")
  in

  let driver_options = {
    filename = filepath
  ; argv = filepath :: args
  ; print_ast = options.print_ast
  ; print_renamed = options.print_renamed
  ; backend = options.backend
  } in
  handle_errors fatal_error (fun _ -> 
    Driver.run driver_options (Lexing.from_channel (open_in filepath)))

let run_repl (options : run_options) : unit =
  Sys.catch_break true;
  let _ = match options.backend with
    | EvalBackend -> ()
    | BytecodeBackend -> fatal_error "The bytecode backend does not support interactive evaluation"
  in
  let driver_options = {
      filename = "<interactive>"
      (* argv.(0) is "", to signify that this is a repl process.
         This is in line with what python does. *)
    ; argv = "" :: List.tl (Array.to_list Sys.argv)
    ; print_ast = options.print_ast
    ; print_renamed = options.print_renamed
    ; backend = options.backend
    } in
  let rec go env scope =
    try
      handle_errors (fun msg -> repl_error msg; go env scope)
        (fun _ -> 
          let prompt = "\x1b[38;5;160m\x02λ>\x1b[0m\x02 " in
          match Readline.readline prompt with
          | None -> exit 0
          | Some input -> 
            let result, new_env, new_scope = Driver.run_env driver_options (Lexing.from_string input) env scope in

            begin match result with
            | UnitV -> ()
            | _ -> print_endline (" - " ^ Value.pretty result)
            end;
            go new_env new_scope)
    with
    | Sys.Break -> 
      go env scope
  in
  go (EvalInst.empty_eval_env driver_options.argv) Rename.RenameScope.empty

  

let usage_message = "usage: polaris [options] [FILE]"


let () =
  let args = ref [] in
  let anon_fun x = args := x :: !args in

  let print_ast = ref false in
  let print_renamed = ref false in
  let backend = ref "eval" in

  let speclist = [
    ("--print-ast", Arg.Set print_ast, "Print the parsed syntax tree before renaming");
    ("--print-renamed", Arg.Set print_renamed, "Print the renamed syntax tree before evaluation");
    ("--backend", Arg.Set_string backend, "The backend used for evaluation. Possible values: 'eval', 'bytecode'")
  ] in
  Arg.parse speclist anon_fun usage_message;
  
  let options = {
      print_ast = !print_ast;
      print_renamed = !print_renamed;
      backend = match !backend with
      | "eval" -> EvalBackend
      | "bytecode" -> BytecodeBackend
      | _ -> fatal_error ("Invalid or unsupported backend: '" ^ !backend ^ "'")
    } in
  match List.rev !args with
  | [] -> run_repl options
  | (filepath :: args) -> ignore (run_file options filepath args)
