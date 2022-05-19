open Ast
open Util

module VarMap = Map.Make (Name)

module MapVImpl = Map.Make (String)

(* `eval_env` unfortunately has to be defined outside of `Eval.Make`,
   since `value` depends on it. *)
type eval_env = { vars : value ref VarMap.t; argv : string list }

and value =
  | StringV of string
  | NumV of float
  (* The closure environment has to be lazy to
     support recursive lets, since the definition of a recursive function has
     to be stored in its own closure, which also stores its on environment, etc.*)
  | ClosureV of eval_env lazy_t * name list * name_expr
  (* PrimOps should be mostly indistinguishable from regular closures.
     The only exception is pretty printing, where primops are printed as
     "<primative: name>" instead of <closure(args)>
     Also, primop names are represented as strings, not names, since
     they are definitely unique and there is no point in keeping
     an additional index *)
  | PrimOpV of string
  | UnitV
  | BoolV of bool
  (* Lists are *immutable*. 
     Otherwise we would have to deal with some kind of a 'place' system, which is
     anything but ideal. One should be able to approximate mutable lists
     by mutable references to immutable lists 99% of the time, so this is
     hopefully not going to be an issue. *)
  | ListV of value list
  | MapV of (value MapVImpl.t)
  | NullV

module EvalError = struct
  exception DynamicVarNotFound of name * loc
  exception NotAValueOfType of string * value * string * loc
  exception TryingToApplyNonFunction of value * loc
  exception TryingToLookupInNonMap of value * string * loc
  exception MapDoesNotContain of value MapVImpl.t * string * loc
  exception InvalidNumberOfArguments of name list * value list * loc
  (* TODO: Once exceptions are implemented, prim op argument errors should
    just be polaris exceptions. 
    Maybe these could even just be contract failures, if those ever
    become a feature? *)
  exception PrimOpArgumentError of string * value list * string * loc

  exception InvalidOperatorArgs of string * value list * loc

  exception InvalidProcessArg of value * loc

  exception NonProgCallInPipe of NameExpr.expr * loc
end  

module Value = struct
  type t = value

  let rec pretty (x : t) : string =
    match x with
    | StringV s -> s
    | NumV n -> 
      if Float.is_integer n
      then string_of_int (int_of_float n)
      else string_of_float n
    | ClosureV (_, params, _) ->
        "<closure(" ^ String.concat ", " (List.map Name.pretty params) ^ ")>"
    | PrimOpV name ->
        "<primative: " ^ name ^ ">"
    | UnitV -> "()"
    | NullV -> "null"
    | BoolV b -> string_of_bool b
    | ListV vals -> "[" ^ String.concat ", " (List.map pretty vals) ^ "]"
    | MapV kvs -> 
      let kv_list = List.of_seq (MapVImpl.to_seq kvs) in
      "#{" ^ String.concat ", " (List.map (fun (k, v) -> k ^ ": " ^ pretty v) kv_list) ^ "}"

  let rec as_args (fail : t -> 'a) (x : t) : string list =
    match x with
    | StringV _ | NumV _ | BoolV _ -> [pretty x]
    (* TODO: Should Maps be converted to JSON? *)
    | ClosureV _ | PrimOpV _ | UnitV | NullV | MapV _ -> fail x
    | ListV x -> List.concat_map (as_args fail) x
end


module Make (Require : sig
  val eval_require : string -> value
end) = struct

  let lookup_var (env : eval_env) (loc : loc) (var : name) : value ref = 
    try 
      VarMap.find var env.vars
    with
      Not_found -> raise (EvalError.DynamicVarNotFound (var, loc))

  let insert_vars (vars : name list) (vals : value list) (env : eval_env) (loc : loc) : eval_env =
    if (List.compare_lengths vars vals != 0) 
    then raise (EvalError.InvalidNumberOfArguments (vars, vals, loc))
    else { env with vars = List.fold_right2 (fun x v r -> VarMap.add x (ref v) r) vars vals env.vars }

  (* Tries to convert a number to a value. Throws an error if it can't *)
  let as_num context loc = function
    | NumV x -> x
    | x -> raise (EvalError.NotAValueOfType ("number", x, context, loc))

  let as_bool context loc = function
    | BoolV x -> x
    | x -> raise (EvalError.NotAValueOfType ("boolean", x, context, loc))

  let as_string context loc = function
    | StringV x -> x
    | x -> raise (EvalError.NotAValueOfType ("string", x, context, loc))

  let rec val_eq (x : value) (y : value) : bool = 
    match x, y with
    | StringV x, StringV y -> String.compare x y == 0;
    | NumV x, NumV y -> 
      Float.equal x y;
    (* Closure comparison always evaluates to false.
       We're not going to solve the halting problem for this. *)
    | (ClosureV _, _ | _, ClosureV _) -> false
    | UnitV, UnitV -> true
    | BoolV x, BoolV y -> x == y
    (* Lists are compared elements-wise, *not* by reference
      (whatever that would even mean with polaris' (more or less)
      referentially transparent semantics) *)
    | ListV xs, ListV ys ->
      if List.compare_lengths xs ys != 0
      then false
      else List.for_all2 val_eq xs ys
    | MapV m1, MapV m2 ->
      MapVImpl.equal val_eq m1 m2
    | NullV, NullV -> true
    (* Comparisons of different values are always false *)
    | _, _ -> false

  let read_untyped_from_fd (fd : Unix.file_descr) : value =
    let open Unix in
    let chan = in_channel_of_descr fd in

    let result = In_channel.input_all chan in

    if String.length result > 0 && result.[String.length result - 1] == '\n' then 
      StringV (String.sub result 0 (String.length result - 1))
    else 
      StringV result

  let rec close_all = function
    | [] -> ()
    | fd::fds ->
      Unix.close fd;
      close_all fds

  let rec eval_expr (env : eval_env) (expr : name_expr) : value =
    let open NameExpr in
    match expr with
    (* The name index -1 specifies primops *)
    | Var (loc, x) ->
        if x.index == -1 
        then PrimOpV x.name
        else !(lookup_var env loc x)
    | App (loc, f, args) -> (
        match eval_expr env f with
        | ClosureV (clos_env, params, body) ->
            (* Function arguments are evaluated left to right *)
            let arg_vals = List.map (eval_expr env) args in
            eval_expr (insert_vars params arg_vals (Lazy.force clos_env) loc) body
        | PrimOpV prim_name ->
            let arg_vals = List.map (eval_expr env) args in
            eval_primop env prim_name arg_vals loc
        | x ->
            raise (EvalError.TryingToApplyNonFunction (x, loc)))
    | Lambda (_, params, e) -> ClosureV (lazy env, params, e)
    
    | StringLit (_, s) -> StringV s
    | NumLit (_, f)    -> NumV f
    | BoolLit (_, b)   -> BoolV b
    | UnitLit _        -> UnitV
    | NullLit _        -> NullV

    | ListLit (_, exprs) -> 
      let vals = List.map (eval_expr env) exprs in 
      ListV vals
    | MapLit (_, kvs) ->
      let kv_vals = Seq.map (fun (k, e) -> (k, eval_expr env e)) (List.to_seq kvs) in
      MapV (MapVImpl.of_seq kv_vals)

    | MapLookup (loc, map_expr, key) ->
      begin match eval_expr env map_expr with
      | MapV map -> 
        begin match MapVImpl.find_opt key map with
        | Some value -> value
        | None -> raise (EvalError.MapDoesNotContain (map, key, loc))
        end
      | value -> raise (EvalError.TryingToLookupInNonMap (value, key, loc))
      end

    (* TODO: Handle untyped *)
    | Add (loc, e1, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      let context = "Trying to add " ^ Value.pretty v1 ^ " and " ^ Value.pretty v2 in
      NumV (as_num context loc v1 +. as_num context loc v2)
    | Sub (loc, e1, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      let context = "Trying to subtract " ^ Value.pretty v2 ^ " from " ^ Value.pretty v1 in
      NumV (as_num context loc v1 -. as_num context loc v2)
    | Mul (loc, e1, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      let context = "Trying to multiply " ^ Value.pretty v1 ^ " and " ^ Value.pretty v2 in
      NumV (as_num context loc v1 *. as_num context loc v2)
    | Div (loc, e1, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      let context = "Trying to divide " ^ Value.pretty v1 ^ " by " ^ Value.pretty v2 in
      NumV (as_num context loc v1 /. as_num context loc v2)
    | Concat (loc, e1, e2) ->
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      begin match v1, v2 with
      | ListV xs, ListV ys -> ListV (xs @ ys)
      | StringV s1, StringV s2 -> StringV (s1 ^ s2)
      | StringV s1, NumV _ -> StringV(s1 ^ Value.pretty v2)
      | _, _ -> raise (EvalError.InvalidOperatorArgs("..", [v1; v2], loc))
      end 

    | Equals (_, e1, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      BoolV (val_eq v1 v2)

    | LE (loc, e1, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      let context = "Trying to compute " ^ Value.pretty v1 ^ " <= " ^ Value.pretty v2 in
      BoolV (as_num context loc v1 <= as_num context loc v2)
    | GE (loc, e1, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      let context = "Trying to compute " ^ Value.pretty v1 ^ " >= " ^ Value.pretty v2 in
      BoolV (as_num context loc v1 >= as_num context loc v2)
    | LT (loc, e1, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      let context = "Trying to compute " ^ Value.pretty v1 ^ " < " ^ Value.pretty v2 in
      BoolV (as_num context loc v1 < as_num context loc v2)
    | GT (loc, e1, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      let context = "Trying to compute " ^ Value.pretty v1 ^ " > " ^ Value.pretty v2 in
      BoolV (as_num context loc v1 > as_num context loc v2)
    
    | If (loc, e1, e2, e3) ->
      let v1 = eval_expr env e1 in
      let context = "In the condition of an if expression" in
      if as_bool context loc v1 then 
        eval_expr env e2
      else
        eval_expr env e3

    | Seq (_, exprs) -> eval_seq env exprs
    | LetSeq _ | LetRecSeq _ -> raise (Panic "let assignment found outside of sequence expression")

    | Let (loc, x, e1, e2) ->
      eval_expr (insert_vars [x] [eval_expr env e1] env loc) e2
    | LetRec (loc, f, params, e1, e2) ->
      let rec env' = lazy (insert_vars [f] [ClosureV (env', params, e1)] env loc) in
      eval_expr (Lazy.force env') e2
    | Assign (loc, x, e1) ->
      let x_ref = lookup_var env loc x  in
      x_ref := (eval_expr env e1);
      UnitV
    | Print (_, expr) ->
      let value = eval_expr env expr in
      print_endline (Value.pretty value);
      UnitV

    | ProgCall (loc, prog, args) as expr -> 
      eval_expr env (Pipe (loc, [expr]))

      

    | Pipe (loc, []) -> raise (Panic "empty pipe") 

    | Pipe (loc, ((ProgCall _ :: _) as exprs)) -> 
      let progs = progs_of_exprs env exprs in
      let in_chan = Pipe.compose_in progs in
      StringV (In_channel.input_all in_chan)

    | Pipe (loc, (expr :: exprs)) ->
      let output_lines = Value.as_args (fun x -> raise (EvalError.InvalidProcessArg (x, loc))) (eval_expr env expr) in

      let progs = progs_of_exprs env exprs in

      let in_chan = Pipe.compose_in_out progs (fun out_chan ->
          List.iter (fun str -> Out_channel.output_string out_chan (str ^ "\n")) output_lines
        ) in
      StringV (In_channel.input_all in_chan)



  (* This takes a continuation argument in order to stay mutually tail recursive with eval_expr *)
  and eval_seq_cont : 'r. eval_env -> name_expr list -> (eval_env -> (name_expr, value) either -> 'r) -> 'r =
    fun env exprs cont ->
    match exprs with
    | [] -> cont env (Right UnitV)
    | LetSeq (loc, x, e) :: exprs -> 
      eval_seq_cont (insert_vars [x] [(eval_expr env e)] env loc) exprs cont
    | LetRecSeq (loc, f, params, e) :: exprs ->
      let rec env' = lazy (insert_vars [f] [ClosureV (env', params, e)] env loc) in
      eval_seq_cont (Lazy.force env') exprs cont
    
    | [ e ] -> 
      cont env (Left e)

    (* Single program calls are just evaluated like pipes *)
    | ProgCall (loc, _, _) as expr :: exprs ->
      eval_seq_cont env (Pipe (loc, [expr]) :: exprs) cont
    
    (* Pipes in seq exprs inherit the parents stdout. *)
    (* Pipes without value inputs also inherit the parents stdin *)
    | Pipe (loc, ((ProgCall _ :: _) as prog_exprs)) :: exprs -> 
      let progs = progs_of_exprs env prog_exprs in
      Pipe.compose progs;
      eval_seq_cont env exprs cont
    | Pipe (loc, (expr :: prog_exprs)) :: exprs ->
      let output_lines = Value.as_args (fun x -> raise (EvalError.InvalidProcessArg (x, loc))) (eval_expr env expr) in
      
      let progs = progs_of_exprs env prog_exprs in
      
      Pipe.compose_out_with progs (fun out_chan -> 
          List.iter (fun line -> Out_channel.output_string out_chan (line ^ "\n")) output_lines
        );
      eval_seq_cont env exprs cont

    | e :: exprs ->
        (* The result of 'eval_expr e' is purposefully ignored *)
        let _ = eval_expr env e in
        eval_seq_cont env exprs cont

  and eval_seq (env : eval_env) (exprs : name_expr list) : value = 
    eval_seq_cont env exprs 
      (fun env expr_or_val -> 
        match expr_or_val with
        | Left expr -> eval_expr env expr
        | Right value -> value
        )

  
  and eval_seq_state (env : eval_env) (exprs : name_expr list) : value * eval_env = 
    eval_seq_cont env exprs 
      (fun env expr_or_val -> 
        match expr_or_val with 
        | Left expr -> (eval_expr env expr, env)
        | Right value -> (value, env))

  and eval_primop env op args loc = let open EvalError in
    (* TODO: intern primop names *)
    match op with
    | "head" -> begin match args with
                | [ListV (head::tail)] -> head
                | [ListV []] -> raise (PrimOpArgumentError ("head", args, "Empty list", loc))
                | _ -> raise (PrimOpArgumentError ("head", args, "Expected a single list", loc))
                end
    | "tail" -> begin match args with
                | [ListV (head::tail)] -> ListV tail
                | [ListV []] -> raise (PrimOpArgumentError ("tail", args, "Empty list", loc))
                | _ -> raise (PrimOpArgumentError ("tail", args, "Expected a single list", loc))
                end
    | "cons" -> begin match args with
                | [x; ListV xs] -> ListV (x :: xs)
                | _ -> raise (PrimOpArgumentError("cons", args, "Expected an element and a list", loc))
                end
    | "require" -> begin match args with
                  (* This just calls out to Require.eval_require.
                     The main reason for doing this is to avoid cyclical imports between Driver and Eval,
                     but this also comes with the nice side effect of allowing
                     the calling code (the driver) to change the way module lookup is performed. *)
                  | [StringV modPath] -> 
                      Require.eval_require modPath
                  | _ -> raise (PrimOpArgumentError ("require", args, "Expected a single string argument", loc))
                  end
    | "lines" -> begin match args with
                | [arg] -> 
                  let context = "Trying to apply 'lines'" in
                  ListV (List.map (fun s -> StringV s) (String.split_on_char '\n' (as_string context loc arg)))
                | _ -> raise (PrimOpArgumentError ("lines", args, "Expected a single string", loc))
                end
    | "replace" -> begin match args with
                  | [needle_v; repl_v; str_v] -> 
                    let context = "Trying to apply 'replace'" in
                    let needle = as_string context loc needle_v in 
                    let repl =  as_string context loc repl_v in
                    let str = as_string context loc str_v in
                    StringV (Str.global_replace (Str.regexp_string needle) repl str)
                  | _ -> raise (PrimOpArgumentError ("replace", args, "Expected three strings", loc))
                  end
    | "regexpReplace" -> begin match args with
                  | [needle_v; repl_v; str_v] -> 
                    let context = "Trying to apply 'regexpReplace'" in
                    let needle = as_string context loc needle_v in 
                    let repl =  as_string context loc repl_v in
                    let str = as_string context loc str_v in
                    StringV (Str.global_replace (Str.regexp needle) repl str)
                  | _ -> raise (PrimOpArgumentError ("regexpReplace", args, "Expected three strings", loc))
                  end
    | "writeFile" -> begin match args with
                  | [path_v; content_v] ->
                    let context = "Trying to apply 'writeFile'" in
                    let path = as_string context loc path_v in
                    let content = as_string context loc content_v in

                    let channel = open_out path in
                    Out_channel.output_string channel content;
                    Out_channel.close channel;
                    UnitV
                  | _ -> raise (PrimOpArgumentError ("writeFile", args, "Expected two string arguments", loc))
                  end
    | "parseInt" -> begin match args with
                  | [StringV arg_str] ->
                    begin match int_of_string_opt (String.trim arg_str) with
                    |  Some(int_val) -> NumV (float_of_int int_val)
                    |  None -> NullV
                    end
                  | _ -> raise (PrimOpArgumentError ("parseInt", args, "Expected a strings", loc))
                  end
    | "parseNum" -> begin match args with
                  | [StringV arg_str] ->
                    begin match float_of_string_opt (String.trim arg_str) with
                    | Some(float_val) -> NumV float_val
                    | None -> NullV
                    end
                  | _ -> raise (PrimOpArgumentError ("parseNum", args, "Expected a strings", loc))
                  end
    | "readLine" -> let prompt = match args with
                    | [] -> ""
                    | [StringV prompt] -> prompt
                    | _ -> raise (PrimOpArgumentError ("readLine", args, "Expected no arguments or a single string", loc))
                    in
                    begin match Readline.readline prompt with
                    | Some input -> StringV input
                    | None -> NullV
                    end
    | "chdir" ->  begin match args with
                  | [StringV path_str] -> 
                    Sys.chdir path_str;
                    UnitV
                  | _ -> raise (PrimOpArgumentError ("chdir", args, "Expected a strings", loc))
                  end
    | "exit" -> begin match args with
                | [NumV arg] ->
                  exit (int_of_float arg)
                | _ -> raise (PrimOpArgumentError ("exit", args, "Expected an integer", loc))
                end
    | "toString" -> begin match args with
                | [NumV arg] -> 
                  (* We have to use `Value.pretty` instead of `Float.to_string`, since
                     the latter always appends a trailing dot. *)
                  StringV (Value.pretty (NumV arg))
                | _ -> raise (PrimOpArgumentError ("toString", args, "Expected a number", loc))
                end            
    | "getArgv" -> begin match args with
                   | [] -> ListV (List.map (fun x -> StringV x) env.argv)
                   | _ -> raise (PrimOpArgumentError ("getArgv", args, "Expected no arguments", loc))
                   end
    | "getEnv" -> begin match args with
                  | [StringV var] -> begin match Sys.getenv_opt var with
                    | None -> NullV
                    | Some(value) -> StringV value
                    end
                  | _ -> raise (PrimOpArgumentError ("getEnv", args, "Expected a single string", loc))
                  end
    | _ -> raise (Panic ("Invalid or unsupported primop: " ^ op))

  and progs_of_exprs env = function
    | ProgCall (loc, progName, args) :: exprs ->
      let fail x = raise (EvalError.InvalidProcessArg (x, loc)) in
      let arg_strings = List.concat_map (fun arg -> Value.as_args fail (eval_expr env arg)) args in
      (progName, arg_strings) :: progs_of_exprs env exprs
    | expr :: exprs -> 
      raise (EvalError.NonProgCallInPipe (expr, NameExpr.get_loc expr))
    | [] -> []

  let empty_eval_env (argv: string list): eval_env = {
    vars = VarMap.empty;
    argv = argv
  }

  let eval (argv : string list) (exprs : name_expr list) : value = eval_seq (empty_eval_env argv) exprs
end

(* Note [left-to-right evaluation]
We sometimes have to explicitly bind variables to intermediary
variables, since OCaml's evaluation order is right-to-left,
but we would like to enforce left-to-right evaluation.
Example:
```
let v1 = eval_expr env e1
let v2 = eval_expr env e2
f v1 v2
```
instead of 
```
f (eval_expr env e1) (eval_expr env e2)
```
*)
