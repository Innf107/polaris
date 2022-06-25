open Syntax
open Util

module VarMap = Map.Make (Name)

module MapVImpl = Map.Make (String)

(* `eval_env` unfortunately has to be defined outside of `Eval.Make`,
   since `value` depends on it. *)
type eval_env = { 
  vars : value ref VarMap.t
; argv : string list 
; call_trace : loc list
}

and value =
  | StringV of string
  | NumV of float
  (* The closure environment has to be lazy to
     support recursive lets, since the definition of a recursive function has
     to be stored in its own closure, which also stores its on environment, etc.*)
  | ClosureV of eval_env lazy_t * name list * Renamed.expr
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
  (* Represents a concurrent thread of execution *)
  | PromiseV of value Promise.t

module EvalError = struct
  exception DynamicVarNotFound of name * loc list
  exception NotAValueOfType of string * value * string * loc list
  exception TryingToApplyNonFunction of value * loc list
  exception TryingToLookupInNonMap of value * string * loc list
  exception TryingToLookupDynamicInNonMap of value * loc list
  exception InvalidKey of value * value MapVImpl.t * loc list
  exception MapDoesNotContain of value MapVImpl.t * string * loc list
  exception InvalidNumberOfArguments of name list * value list * loc list
  (* TODO: Once exceptions are implemented, prim op argument errors should
    just be polaris exceptions. 
    Maybe these could even just be contract failures, if those ever
    become a feature? *)
  exception PrimOpArgumentError of string * value list * string * loc list
  exception PrimOpError of string * string * loc list

  exception InvalidOperatorArgs of string * value list * loc list

  exception NonNumberInRangeBounds of value * value * loc list

  exception NonBoolInListComp of value * loc list
  exception NonListInListComp of value * loc list

  exception InvalidProcessArg of value * loc list

  exception NonProgCallInPipe of Renamed.expr * loc list

  exception RuntimeError of string * loc list

  exception ModuleNotFound of string * string list

  exception AwaitNonPromise of value * loc list

  exception ArgParseError of string

  exception NonExhaustiveMatch of value * loc list

  exception EnsureFailed of string * loc list
end  

module Value = struct
  type t = value

  let rec pretty (x : t) : string =
    match x with
    | StringV s -> "\"" ^ s ^ "\""
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
    | PromiseV p ->
      match Promise.peek p with
      | Finished value -> "<promise: " ^ pretty value ^ ">"
      | Failed _ex -> "<promise: <<failure>>>" 
      | Pending -> "<promise: <<pending>>>"

  let rec as_args (fail : t -> 'a) (x : t) : string list =
    match x with
    | StringV v -> [v]
    | NumV _ | BoolV _ -> [pretty x]
    (* TODO: Should Maps be converted to JSON? *)
    | ClosureV _ | PrimOpV _ | UnitV | NullV | MapV _ | PromiseV _ -> fail x
    | ListV x -> List.concat_map (as_args fail) x
end


module Make (Require : sig
  val eval_require : string -> string -> value
end) = struct

  let lookup_var (env : eval_env) (loc : loc) (var : name) : value ref = 
    try 
      VarMap.find var env.vars
    with
      Not_found -> raise (EvalError.DynamicVarNotFound (var, loc :: env.call_trace))

  let insert_vars (vars : name list) (vals : value list) (env : eval_env) (loc : loc) : eval_env =
    if (List.compare_lengths vars vals != 0) 
    then raise (EvalError.InvalidNumberOfArguments (vars, vals, loc :: env.call_trace))
    else { env with vars = List.fold_right2 (fun x v r -> VarMap.add x (ref v) r) vars vals env.vars }

  let insert_var : name -> value -> eval_env -> eval_env * value ref =
    fun var value env ->
      let var_ref = ref value in
      { env with vars = VarMap.add var var_ref env.vars }, var_ref

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

  let rec match_pat (pat : Renamed.pattern) (scrut : value) : (eval_env -> eval_env) option =
    let (let*) = Option.bind in
    match pat, scrut with
    | VarPat (_, x), scrut ->
      Some(fun env -> fst(insert_var x scrut env))
    | ConsPat(_, p, ps), ListV (v :: vs) ->
      let* x_trans = match_pat p v in
      let* xs_trans = match_pat ps (ListV vs) in
      Some(fun env -> xs_trans (x_trans env)) 
    | ListPat(_, ps), ListV (vs) -> 
      if List.compare_lengths ps vs != 0 then
        None
      else
        let* transformations = Util.sequence_options (List.map2 match_pat ps vs) in
        Some(List.fold_right (fun t r env -> t (r env)) transformations (fun x -> x))
    | NumPat(_, f1), NumV f2 when Float.equal f1 f2 ->
        Some (fun x -> x) 
    | OrPat(_, p1, p2), v ->
      begin match match_pat p1 v with
      | Some(t) -> Some(t)
      | None -> match_pat p2 v
      end
    | _ -> None

  let rec eval_expr (env : eval_env) (expr : Renamed.expr) : value =
    let open Renamed in
    match expr with
    (* The name index -1 specifies primops *)
    | Var (loc, x) ->
        if x.index == -1 
        then PrimOpV x.name
        else !(lookup_var env loc x)
    | App (loc, f, args) ->
      (* See Note [left-to-right evaluation] *)
      let f_val = eval_expr env f in
      let arg_vals = List.map (eval_expr env) args in
      eval_app env loc f_val arg_vals
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
        | None -> raise (EvalError.MapDoesNotContain (map, key, loc :: env.call_trace))
        end
      | value -> raise (EvalError.TryingToLookupInNonMap (value, key, loc :: env.call_trace))
      end
    | DynLookup (loc, map_expr, key_expr) ->
      begin match eval_expr env map_expr with
      | MapV map -> 
        begin match eval_expr env key_expr with
        | StringV key -> begin match MapVImpl.find_opt key map with
                         | Some value -> value
                         | None -> NullV
                         end
        | value -> raise (EvalError.InvalidKey (value, map, loc :: env.call_trace))
        end
      | value -> raise (EvalError.TryingToLookupDynamicInNonMap (value, loc :: env.call_trace))
      end

    (* TODO: Handle untyped *)
    | Add (loc, e1, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      let context = "Trying to add " ^ Value.pretty v1 ^ " and " ^ Value.pretty v2 in
      NumV (as_num context (loc :: env.call_trace) v1 +. as_num context (loc :: env.call_trace) v2)
    | Sub (loc, e1, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      let context = "Trying to subtract " ^ Value.pretty v2 ^ " from " ^ Value.pretty v1 in
      NumV (as_num context (loc :: env.call_trace) v1 -. as_num context (loc :: env.call_trace) v2)
    | Mul (loc, e1, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      let context = "Trying to multiply " ^ Value.pretty v1 ^ " and " ^ Value.pretty v2 in
      NumV (as_num context (loc :: env.call_trace) v1 *. as_num context (loc :: env.call_trace) v2)
    | Div (loc, e1, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      let context = "Trying to divide " ^ Value.pretty v1 ^ " by " ^ Value.pretty v2 in
      NumV (as_num context (loc :: env.call_trace) v1 /. as_num context (loc :: env.call_trace) v2)
    | Concat (loc, e1, e2) ->
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      begin match v1, v2 with
      | ListV xs, ListV ys -> ListV (xs @ ys)
      | MapV xs, MapV ys -> MapV (MapVImpl.union (fun _ _ y -> Some y) xs ys)
      | StringV s1, StringV s2 -> StringV (s1 ^ s2)
      | StringV s1, NumV _ -> StringV(s1 ^ Value.pretty v2)
      | NumV _, StringV s2 -> StringV(Value.pretty v1 ^ s2)
      | _, _ -> raise (EvalError.InvalidOperatorArgs("~", [v1; v2], loc :: env.call_trace))
      end 

    | Equals (_, e1, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      BoolV (val_eq v1 v2)
    | NotEquals (_, e1, e2) -> 
        (* See Note [left-to-right evaluation] *)
        let v1 = eval_expr env e1 in
        let v2 = eval_expr env e2 in
        BoolV (not (val_eq v1 v2))

    | LE (loc, e1, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      let context = "Trying to compute " ^ Value.pretty v1 ^ " <= " ^ Value.pretty v2 in
      BoolV (as_num context (loc :: env.call_trace) v1 <= as_num context (loc :: env.call_trace) v2)
    | GE (loc, e1, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      let context = "Trying to compute " ^ Value.pretty v1 ^ " >= " ^ Value.pretty v2 in
      BoolV (as_num context (loc :: env.call_trace) v1 >= as_num context (loc :: env.call_trace) v2)
    | LT (loc, e1, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      let context = "Trying to compute " ^ Value.pretty v1 ^ " < " ^ Value.pretty v2 in
      BoolV (as_num context (loc :: env.call_trace) v1 < as_num context (loc :: env.call_trace) v2)
    | GT (loc, e1, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      let context = "Trying to compute " ^ Value.pretty v1 ^ " > " ^ Value.pretty v2 in
      BoolV (as_num context (loc :: env.call_trace) v1 > as_num context (loc :: env.call_trace) v2)
    
    | Or(loc, e1, e2) ->
      begin match eval_expr env e1 with
      | BoolV true -> BoolV true
      | BoolV false -> 
        begin match eval_expr env e2 with 
        | BoolV true -> BoolV true
        | BoolV false -> BoolV false
        | value -> raise (EvalError.NotAValueOfType ("bool", value, "In the second argument of an || expression", loc :: env.call_trace))
        end
      | value -> raise (EvalError.NotAValueOfType ("bool", value, "In the first argument of an || expression", loc :: env.call_trace))
      end
    | And(loc, e1, e2) ->
        begin match eval_expr env e1 with
        | BoolV false -> BoolV false
        | BoolV true -> 
          begin match eval_expr env e2 with 
          | BoolV true -> BoolV true
          | BoolV false -> BoolV false
          | value -> raise (EvalError.NotAValueOfType ("bool", value, "In the second argument of an || expression", loc :: env.call_trace))
          end
        | value -> raise (EvalError.NotAValueOfType ("bool", value, "In the first argument of an && expression", loc :: env.call_trace))
        end
    | Not(loc, e) ->
        begin match eval_expr env e with
        | BoolV b -> BoolV (not b)
        | value -> raise (EvalError.NotAValueOfType("bool", value, "In the argument of a 'not' expression", loc :: env.call_trace))
        end
    
    | Range(loc, e1, e2) ->
      let start_val = eval_expr env e1 in
      let end_val = eval_expr env e2 in
      begin match start_val, end_val with
      | NumV start_num, NumV end_num ->
        let rec build_range acc x = 
          if x < start_num then
            acc
          else
            build_range (NumV x::acc) (x -. 1.)
          in
        ListV (build_range [] end_num) 
      | _ -> raise (EvalError.NonNumberInRangeBounds(start_val, end_val, loc :: env.call_trace))
      end
    | ListComp (loc, result_expr, comp_exprs) ->
      ListV (eval_list_comp env loc result_expr comp_exprs)
    | If (loc, e1, e2, e3) ->
      let v1 = eval_expr env e1 in
      let context = "In the condition of an if expression" in
      if as_bool context (loc :: env.call_trace) v1 then 
        eval_expr env e2
      else
        eval_expr env e3

    | Seq (_, exprs) -> eval_seq env exprs
    | LetSeq _ | LetRecSeq _ -> raise (Panic "let assignment found outside of sequence expression")

    | Let (loc, pat, e1, e2) ->
      let scrut = eval_expr env e1 in
      begin match match_pat pat scrut with
      | Some (env_trans) -> eval_expr (env_trans env) e2
      | None -> raise (EvalError.NonExhaustiveMatch (scrut, loc :: env.call_trace))
      end
      (* eval_expr (insert_vars [x] [eval_expr env e1] env loc) e2 *)
    | LetRec (loc, f, params, e1, e2) ->
      let rec env' = lazy (insert_vars [f] [ClosureV (env', params, e1)] env loc) in
      eval_expr (Lazy.force env') e2
    | Assign (loc, x, e1) ->
      let x_ref = lookup_var env loc x  in
      x_ref := (eval_expr env e1);
      UnitV

    | ProgCall (loc, prog, args) as expr -> 
      eval_expr env (Pipe (loc, [expr]))

      

    | Pipe (loc, []) -> raise (Panic "empty pipe") 

    | Pipe (loc, ((ProgCall _ :: _) as exprs)) -> 
      let progs = progs_of_exprs env exprs in
      let in_chan = Pipe.compose_in progs in
      StringV (String.trim (In_channel.input_all in_chan))

    | Pipe (loc, (expr :: exprs)) ->
      let output_lines = Value.as_args (fun x -> raise (EvalError.InvalidProcessArg (x, loc :: env.call_trace))) (eval_expr env expr) in

      let progs = progs_of_exprs env exprs in

      let in_chan = Pipe.compose_in_out progs (fun out_chan ->
          List.iter (fun str -> Out_channel.output_string out_chan (str ^ "\n")) output_lines
        ) in
      StringV (String.trim (In_channel.input_all in_chan))
    | Async (loc, expr) ->
      let promise = Promise.create begin fun _ ->
        eval_expr env expr
      end in
      PromiseV promise
    | Await (loc, expr) ->
      begin match eval_expr env expr with
      | PromiseV p -> 
        Promise.await p
      | value -> raise (EvalError.AwaitNonPromise (value, loc :: env.call_trace))
      end
    | Match (loc, scrut, branches) ->
      let scrut_val = eval_expr env scrut in
      let rec go = function
        | (pat, expr) :: branches -> 
          begin match match_pat pat scrut_val with
          | Some env_trans ->
            eval_expr (env_trans env) expr
          | None -> go branches
          end
        | [] -> raise (EvalError.NonExhaustiveMatch (scrut_val, loc :: env.call_trace))
      in
      go branches

  and eval_app env loc fun_v arg_vals = 
    match fun_v with
    | ClosureV (clos_env, params, body) ->
        (* Function arguments are evaluated left to right *)
        let updated_clos_env = insert_vars params arg_vals (Lazy.force clos_env) loc in
        (* The call trace is extended and carried over to the closure environment *)
        eval_expr {updated_clos_env with call_trace = loc :: env.call_trace} body
    
    | PrimOpV prim_name ->
        eval_primop {env with call_trace = loc :: env.call_trace} prim_name arg_vals loc
    | x ->
        raise (EvalError.TryingToApplyNonFunction (x, loc :: env.call_trace))



  (* This takes a continuation argument in order to stay mutually tail recursive with eval_expr *)
  and eval_seq_cont : 'r. eval_env -> Renamed.expr list -> (eval_env -> (Renamed.expr, value) either -> 'r) -> 'r =
    fun env exprs cont ->
    match exprs with
    | [] -> cont env (Right UnitV)
    | LetSeq (loc, pat, e) :: exprs -> 
      let scrut = eval_expr env e in
      begin match match_pat pat scrut with
      | Some env_trans -> eval_seq_cont (env_trans env) exprs cont
      | None -> raise (EvalError.NonExhaustiveMatch (scrut, loc :: env.call_trace))
      end
    | LetRecSeq (loc, f, params, e) :: exprs ->
      let rec env' = lazy (insert_vars [f] [ClosureV (env', params, e)] env loc) in
      eval_seq_cont (Lazy.force env') exprs cont
    
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
      let output_lines = Value.as_args (fun x -> raise (EvalError.InvalidProcessArg (x, loc :: env.call_trace))) (eval_expr env expr) in
      
      let progs = progs_of_exprs env prog_exprs in
      
      Pipe.compose_out_with progs (fun out_chan -> 
          List.iter (fun line -> Out_channel.output_string out_chan (line ^ "\n")) output_lines
        );
      eval_seq_cont env exprs cont

    | [ e ] -> 
      cont env (Left e)  
    | e :: exprs ->
        (* The result of 'eval_expr e' is purposefully ignored *)
        let _ = eval_expr env e in
        eval_seq_cont env exprs cont

  and eval_seq (env : eval_env) (exprs : Renamed.expr list) : value = 
    eval_seq_cont env exprs 
      (fun env expr_or_val -> 
        match expr_or_val with
        | Left expr -> eval_expr env expr
        | Right value -> value
        )

  
  and eval_seq_state (env : eval_env) (exprs : Renamed.expr list) : value * eval_env = 
    eval_seq_cont env exprs 
      (fun env expr_or_val -> 
        match expr_or_val with 
        | Left expr -> (eval_expr env expr, env)
        | Right value -> (value, env))

  (* TODO: This should probably be tail recursive if possible (probably via CPS) *)
  and eval_list_comp env loc result_expr = function
    | Renamed.FilterClause expr :: comps -> 
      begin match eval_expr env expr with
      | BoolV false -> []
      | BoolV true -> eval_list_comp env loc result_expr comps
      | v -> raise (EvalError.NonBoolInListComp (v, loc :: env.call_trace))
      end
    | DrawClause (name, expr) :: comps ->
      begin match eval_expr env expr with
      | ListV values ->
        let eval_with_val v =
          let env' = insert_vars [name] [v] env loc in
          eval_list_comp env' loc result_expr comps
        in
        List.concat_map eval_with_val values
      | v -> raise (EvalError.NonListInListComp (v, loc :: env.call_trace))
      end
    | [] -> 
      [eval_expr env result_expr]

  and eval_primop env op args loc = let open EvalError in
    (* TODO: intern primop names *)
    match op with
    | "print" ->
      let pretty_print = function
      | StringV str -> str
      | x -> Value.pretty x
      in
      print_endline (String.concat " " (List.map pretty_print args));
      UnitV
    | "head" -> begin match args with
                | [ListV (head::tail)] -> head
                | [ListV []] -> raise (PrimOpArgumentError ("head", args, "Empty list", loc :: env.call_trace))
                | _ -> raise (PrimOpArgumentError ("head", args, "Expected a single list", loc :: env.call_trace))
                end
    | "tail" -> begin match args with
                | [ListV (head::tail)] -> ListV tail
                | [ListV []] -> raise (PrimOpArgumentError ("tail", args, "Empty list", loc :: env.call_trace))
                | _ -> raise (PrimOpArgumentError ("tail", args, "Expected a single list", loc :: env.call_trace))
                end
    | "cons" -> begin match args with
                | [x; ListV xs] -> ListV (x :: xs)
                | _ -> raise (PrimOpArgumentError("cons", args, "Expected an element and a list", loc :: env.call_trace))
                end
    | "require" -> begin match args with
                  (* This just calls out to Require.eval_require.
                     The main reason for doing this is to avoid cyclical imports between Driver and Eval,
                     but this also comes with the nice side effect of allowing
                     the calling code (the driver) to change the way module lookup is performed. *)
                  | [StringV modPath] -> 
                    let cwd = match List.hd (env.argv) with
                    | "" -> Sys.getcwd ()
                    | p -> Filename.dirname p
                    in
                    Require.eval_require cwd modPath
                  | _ -> raise (PrimOpArgumentError ("require", args, "Expected a single string argument", loc :: env.call_trace))
                  end
    | "lines" -> begin match args with
                | [StringV ""] -> ListV []
                | [StringV arg] -> 
                  ListV (List.map (fun s -> StringV s) (String.split_on_char '\n' arg))
                | _ -> raise (PrimOpArgumentError ("lines", args, "Expected a single string", loc :: env.call_trace))
                end
    | "replace" -> begin match args with
                  | [needle_v; repl_v; str_v] -> 
                    let context = "Trying to apply 'replace'" in
                    let needle = as_string context (loc :: env.call_trace) needle_v in 
                    let repl =  as_string context (loc :: env.call_trace) repl_v in
                    let str = as_string context (loc :: env.call_trace) str_v in
                    StringV (Str.global_replace (Str.regexp_string needle) repl str)
                  | _ -> raise (PrimOpArgumentError ("replace", args, "Expected three strings", loc :: env.call_trace))
                  end
    | "regexpReplace" -> begin match args with
                  | [StringV pattern; StringV repl; StringV str] -> 
                    StringV (Pcre.replace ~pat:pattern ~templ:repl str)
                  | _ -> raise (PrimOpArgumentError ("regexpReplace", args, "Expected three strings", loc :: env.call_trace))
                  end
    | "regexpMatch" -> begin match args with
                  | [StringV pattern; StringV arg] ->
                    let regexp = Pcre.regexp pattern in
                    begin try
                      let results = Pcre.exec_all ~rex:regexp ~flags:[] arg in
                      ListV (List.map (fun x -> StringV (Pcre.get_substring x 0)) (Array.to_list results))
                    with
                    | Not_found -> ListV []
                    end

                  | _ -> raise (PrimOpArgumentError ("regexpMatch", args, "Expected (string, string)", loc :: env.call_trace))
                  end
    | "regexpTransform" -> begin match args with
                  | [StringV pattern; transformClos; StringV str_v] ->
                    begin try
                      let results = Pcre.exec_all ~pat:pattern str_v in
                      let rec go pos = function
                      | (substr::results) ->
                        let start_pos, end_pos = Pcre.get_substring_ofs substr 0 in

                        let matched = Pcre.get_substring substr 0 in

                        let replacement = match eval_app env loc transformClos [StringV matched] with
                        | StringV repl -> repl
                        | value -> raise (PrimOpError ("regexpTransform", "Replacement function did not return a string. Returned value: " ^ Value.pretty value, loc :: env.call_trace))
                        in

                        let start_string = String.sub str_v pos (start_pos - pos) in 


                        start_string ^ replacement ^ go end_pos results
                      | [] -> String.sub str_v pos (String.length str_v - pos)
                      in
                      StringV (go 0 (Array.to_list results))
                    with
                    | Not_found -> StringV str_v
                    end
                  | _ -> raise (PrimOpArgumentError ("regexpTransform", args, "Expected (string, function, string)", loc :: env.call_trace))
                  end
    | "writeFile" -> begin match args with
                  | [path_v; content_v] ->
                    let context = "Trying to apply 'writeFile'" in
                    let path = as_string context (loc :: env.call_trace) path_v in
                    let content = as_string context (loc :: env.call_trace) content_v in

                    let channel = open_out path in
                    Out_channel.output_string channel content;
                    Out_channel.close channel;
                    UnitV
                  | _ -> raise (PrimOpArgumentError ("writeFile", args, "Expected two string arguments", loc :: env.call_trace))
                  end
    | "parseInt" -> begin match args with
                  | [StringV arg_str] ->
                    begin match int_of_string_opt (String.trim arg_str) with
                    |  Some(int_val) -> NumV (float_of_int int_val)
                    |  None -> NullV
                    end
                  | _ -> raise (PrimOpArgumentError ("parseInt", args, "Expected a strings", loc :: env.call_trace))
                  end
    | "parseNum" -> begin match args with
                  | [StringV arg_str] ->
                    begin match float_of_string_opt (String.trim arg_str) with
                    | Some(float_val) -> NumV float_val
                    | None -> NullV
                    end
                  | _ -> raise (PrimOpArgumentError ("parseNum", args, "Expected a strings", loc :: env.call_trace))
                  end
    | "readLine" -> let prompt = match args with
                    | [] -> ""
                    | [StringV prompt] -> prompt
                    | _ -> raise (PrimOpArgumentError ("readLine", args, "Expected no arguments or a single string", loc :: env.call_trace))
                    in
                    begin match Bestline.bestline prompt with
                    | Some input -> StringV input
                    | None -> NullV
                    end
    | "chdir" ->  begin match args with
                  | [StringV path_str] -> 
                    Sys.chdir path_str;
                    UnitV
                  | _ -> raise (PrimOpArgumentError ("chdir", args, "Expected a strings", loc :: env.call_trace))
                  end
    | "exit" -> begin match args with
                | [NumV arg] ->
                  exit (int_of_float arg)
                | _ -> raise (PrimOpArgumentError ("exit", args, "Expected an integer", loc :: env.call_trace))
                end
    | "toString" -> begin match args with
                | [arg] -> 
                  (* We have to use `Value.pretty` instead of `Float.to_string`, since
                     the latter always appends a trailing dot. *)
                  StringV (Value.pretty arg)
                | _ -> raise (PrimOpArgumentError ("toString", args, "Expected a number", loc :: env.call_trace))
                end            
    | "getArgv" -> begin match args with
                   | [] -> ListV (List.map (fun x -> StringV x) env.argv)
                   | _ -> raise (PrimOpArgumentError ("getArgv", args, "Expected no arguments", loc :: env.call_trace))
                   end
    | "getEnv" -> begin match args with
                  | [StringV var] -> begin match Sys.getenv_opt var with
                    | None -> NullV
                    | Some(value) -> StringV value
                    end
                  | _ -> raise (PrimOpArgumentError ("getEnv", args, "Expected a single string", loc :: env.call_trace))
                  end
    | "insert" -> begin match args with
                  | [StringV key; value; MapV map] ->
                    MapV (MapVImpl.add key value map)
                  | _ -> raise(PrimOpArgumentError ("insert", args, "Expected a string, a value and a map", loc :: env.call_trace))
                  end
    | "mapToList" -> begin match args with
                  | [MapV map] ->
                    ListV (List.map (fun (k, v) -> ListV [StringV k; v]) (MapVImpl.bindings map))
                  | _ -> raise(PrimOpArgumentError ("mapToList", args, "Expected a single map", loc :: env.call_trace))
                  end
    | "fail" -> begin match args with
                | [StringV msg] -> raise (RuntimeError (msg, loc :: env.call_trace))
                | _ -> raise (PrimOpArgumentError("fail", args, "Expected a string", loc :: env.call_trace))
                end
    | "scriptLocal" -> begin match args with
                | [StringV path] -> 
                  StringV (Filename.dirname (List.hd env.argv) ^ "/" ^ path)
                | _ -> raise (PrimOpArgumentError("fail", args, "Expected a string", loc :: env.call_trace))
                end
    | "commandExists" -> begin match args with
                | [StringV path] ->
                  BoolV (Util.command_exists path)
                | _ -> raise (PrimOpArgumentError("commandExists", args, "Expected a string", loc :: env.call_trace))
                end
    | "ensure" -> begin match args with
                | [StringV path] ->
                  if Util.command_exists path then
                    UnitV
                  else
                    raise (EnsureFailed (path, loc::env.call_trace))
                | _ -> raise (PrimOpArgumentError("ensure", args, "Expected a string", loc :: env.call_trace))
                end
    | _ -> raise (Panic ("Invalid or unsupported primop: " ^ op))

  and progs_of_exprs env = function
    | ProgCall (loc, progName, args) :: exprs ->
      let fail x = raise (EvalError.InvalidProcessArg (x, loc :: env.call_trace)) in
      let arg_strings = List.concat_map (fun arg -> Value.as_args fail (eval_expr env arg)) args in
      (progName, arg_strings) :: progs_of_exprs env exprs
    | expr :: exprs -> 
      raise (EvalError.NonProgCallInPipe (expr, Renamed.get_loc expr :: env.call_trace))
    | [] -> []

  let empty_eval_env (argv: string list): eval_env = {
    vars = VarMap.empty;
    argv = argv;
    call_trace = []
  }

  let eval (argv : string list) (exprs : Renamed.expr list) : value = eval_seq (empty_eval_env argv) exprs

  let flag_info_of_flag_def (env : eval_env) (flag_def : Renamed.flag_def): Argparse.flag_info * eval_env =
    let default_value = match flag_def.arg_count with
    | 0 -> BoolV false
    | -1 -> ListV []
    | _ -> match flag_def.default with
      | Some(str) -> StringV(str)
      | None -> NullV
    in

    let env, flag_ref = insert_var flag_def.flag_var default_value env in


    { aliases = flag_def.flags 
    ; arg_count = if flag_def.arg_count == -1 then 1 else flag_def.arg_count
    ; description = Option.value ~default:"" flag_def.description
    ; action = function
      | [] -> flag_ref := BoolV true
      | [str] -> 
        if flag_def.arg_count == -1 then
          match !flag_ref with
          | ListV vals -> flag_ref := ListV (vals @ [StringV str])
          | _ -> raise (Panic ("Variadic flag with intermediary non-list value: " ^ String.concat ", " flag_def.flags))
        else 
          flag_ref := StringV str
      | strs -> flag_ref := ListV (List.map (fun x -> StringV x) strs)
    }, env

  let eval_header (env : eval_env) (header : Renamed.header) : eval_env =
    let description = Option.value ~default:"" header.description in
    let usage = Option.value ~default: "[OPTIONS]" header.usage in

    let update_option option (env, infos) = 
      let info, env = flag_info_of_flag_def env option in
      (env, info::infos)
    in
    let env, infos = List.fold_right update_option header.options (env, []) in
    
    let prog_info = Argparse.{
      name = List.hd env.argv
    ; description
    ; usage
    } in

    let args = Argparse.run infos prog_info (fun msg -> raise (EvalError.ArgParseError msg)) env.argv in

    { env with argv = (List.hd env.argv :: args) }
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
