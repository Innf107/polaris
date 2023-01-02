open Syntax
open Syntax.Renamed
open Util

module VarMap = Map.Make (Name)

module EnvMap = Map.Make (String)

module RecordVImpl = Multimap.Make (String)

(* `eval_env` unfortunately has to be defined outside of `Eval.Make`,
   since `value` depends on it. *)
type eval_env = { 
  vars : value ref VarMap.t
; env_vars : string EnvMap.t
; argv : string list 
; call_trace : loc list
; last_status : int ref
; module_vars : runtime_module VarMap.t
}

and runtime_module = {
  mod_vars : value ref VarMap.t
; modules : runtime_module VarMap.t
}

and value =
  | StringV of string
  | NumV of float
  (* The closure environment has to be lazy to
     support recursive lets, since the definition of a recursive function has
     to be stored in its own closure, which also stores its on environment, etc.*)
  | ClosureV of eval_env lazy_t * Renamed.pattern list * Renamed.expr
  (* PrimOps should be mostly indistinguishable from regular closures.
     The only exception is pretty printing, where primops are printed as
     "<primative: name>" instead of <closure(args)>
     Also, primop names are represented as strings, not names, since
     they are definitely unique and there is no point in keeping
     an additional index *)
  | PrimOpV of string
  | BoolV of bool
  (* Lists are *immutable*. 
     Otherwise we would have to deal with some kind of a 'place' system, which is
     anything but ideal. One should be able to approximate mutable lists
     by mutable references to immutable lists 99% of the time, so this is
     hopefully not going to be an issue. *)
  | ListV of value list
  | TupleV of value array
  | RecordV of (value RecordVImpl.t)
  | NullV
  (* Represents a concurrent thread of execution *)
  | PromiseV of value Promise.t
  (* Until there are type classes, we need to keep the constructor name
     for pretty printing purposes *)
  | DataConV of name * value
  (* Data constructors can be used like functions *)
  | PartialDataConV of name

let unitV = RecordV (RecordVImpl.empty)
let isUnitV = function
  | RecordV record -> RecordVImpl.equal (fun _ _ -> false) record RecordVImpl.empty
  | _ -> false

module EvalError = struct
  exception DynamicVarNotFound of name * loc list
  exception NotAValueOfType of string * value * string * loc list
  exception TryingToApplyNonFunction of value * loc list
  exception TryingToLookupInNonMap of value * string * loc list
  exception TryingToLookupDynamicInNonMap of value * loc list
  exception InvalidMapKey of value * value RecordVImpl.t * loc list
  exception InvalidListKey of value * value list * loc list
  exception MapDoesNotContain of value RecordVImpl.t * string * loc list
  exception InvalidNumberOfArguments of Renamed.pattern list * value list * loc list
  exception IndexOutOfRange of value list * int * loc list
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
  exception InvalidEnvVarValue of string * value * loc list

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
        "<closure(" ^ String.concat ", " (List.map Syntax.Renamed.pretty_pattern params) ^ ")>"
    | PrimOpV name ->
        "<primative: " ^ name ^ ">"
    | NullV -> "null"
    | BoolV b -> string_of_bool b
    | ListV vals -> "[" ^ String.concat ", " (List.map pretty vals) ^ "]"
    | TupleV vals -> "(" ^ String.concat ", " (Array.to_list (Array.map pretty vals)) ^ ")"
    | RecordV kvs -> 
      let kv_list = List.of_seq (RecordVImpl.to_seq kvs) in
      "{ " ^ String.concat ", " (List.map (fun (k, v) -> k ^ " = " ^ pretty v) kv_list) ^ " }"
    | PromiseV p ->
      begin match Promise.peek p with
      | Finished value -> "<promise: " ^ pretty value ^ ">"
      | Failed _ex -> "<promise: <<failure>>>" 
      | Pending -> "<promise: <<pending>>>"
      end
    | DataConV(constructor_name, value) ->
      constructor_name.name ^ "(" ^ pretty value ^ ")" 
    | PartialDataConV(constructor_name) ->
      "<constructor: " ^ Name.pretty constructor_name ^ ">"


  let rec as_args (fail : t -> 'a) (x : t) : string list =
    match x with
    | StringV v -> [v]
    | NumV _ | BoolV _ -> [pretty x]
    (* TODO: Should records/maps be converted to JSON? *)
    | ClosureV _ | PrimOpV _ | NullV | TupleV _ | RecordV _ | PromiseV _ | DataConV _ | PartialDataConV _ -> fail x
    | ListV x -> List.concat_map (as_args fail) x
end


let lookup_var (env : eval_env) (loc : loc) (var : name) : value ref = 
  try 
    VarMap.find var env.vars
  with
    Not_found -> raise (EvalError.DynamicVarNotFound (var, loc :: env.call_trace))

let insert_var : name -> value -> eval_env -> eval_env * value ref =
  fun var value env ->
    let var_ref = ref value in
    { env with vars = VarMap.add var var_ref env.vars }, var_ref

let insert_env_var : loc -> string -> value -> eval_env -> eval_env =
  fun loc var value env -> 
    match value with 
    | NullV     -> { env with env_vars = EnvMap.remove var env.env_vars }
    | StringV x -> { env with env_vars = EnvMap.add var x env.env_vars }
    | (NumV _ | BoolV _) as v -> { env with env_vars = EnvMap.add var (Value.pretty v) env.env_vars }
    | ClosureV _ | ListV _ | TupleV _ | PrimOpV _ | RecordV _ | PromiseV _ | DataConV _ | PartialDataConV _
      -> raise (EvalError.InvalidEnvVarValue (var, value, loc :: env.call_trace))

let insert_module_var : name -> runtime_module -> eval_env -> eval_env =
  fun var module_value env ->
    { env with module_vars = VarMap.add var module_value env.module_vars }

let full_env_vars : eval_env -> string array =
  fun env ->
    Array.append
    (Unix.environment ()) 
    (Array.of_seq (Seq.map (fun (x, y) -> x ^ "=" ^ y) (EnvMap.to_seq env.env_vars)))

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
  | StringV x, StringV y -> String.compare x y = 0;
  | NumV x, NumV y -> 
    Float.equal x y;
  (* Closure comparison always evaluates to false.
      We're not going to solve the halting problem for this. *)
  | (ClosureV _, _ | _, ClosureV _) -> false
  | BoolV x, BoolV y -> x = y
  (* Lists are compared elements-wise, *not* by reference
    (whatever that would even mean with polaris' (more or less)
    referentially transparent semantics) *)
  | ListV xs, ListV ys ->
    if List.compare_lengths xs ys != 0
    then false
    else List.for_all2 val_eq xs ys
  | RecordV m1, RecordV m2 ->
    RecordVImpl.equal val_eq m1 m2
  | NullV, NullV -> true
  (* Comparisons of different values are always false *)
  | _, _ -> false

let trim_output = function
| "" -> "" 
| str -> match str.[String.length str - 1] with
  | '\n' -> String.sub str 0 (String.length str - 1)
  | _ -> str


let rec match_pat_opt (pat : Renamed.pattern) (scrut : value) : (eval_env -> eval_env) option =
  let (let*) = Option.bind in
  match pat, scrut with
  (* Type patterns are ignored at runtime. These have already been
     checked by the typechecker *)
  | TypePat (_, pat, _), scrut -> match_pat_opt pat scrut
  | VarPat (_, x), scrut ->
    Some(fun env -> fst(insert_var x scrut env))
  | ConsPat(_, p, ps), ListV (v :: vs) ->
    let* x_trans = match_pat_opt p v in
    let* xs_trans = match_pat_opt ps (ListV vs) in
    Some(fun env -> xs_trans (x_trans env)) 
  | ListPat(_, ps), ListV (vs) -> 
    if List.compare_lengths ps vs != 0 then
      None
    else
      let* transformations = Util.sequence_options (List.map2 match_pat_opt ps vs) in
      Some(List.fold_right (fun t r env -> t (r env)) transformations (fun x -> x))
  | TuplePat(_, ps), TupleV vs ->
    let pats = Array.of_list ps in
    (* This check is technically redundant once the type checker is implemented *)
    if Array.length pats != Array.length vs then
      None
    else
      let* transformations = Util.sequence_options_array (Array.map2 match_pat_opt pats vs) in
      Some(Array.fold_right (fun t r env -> t (r env)) transformations (fun x -> x))
    
    | NumPat(_, f1), NumV f2 when Float.equal f1 f2 ->
      Some (fun x -> x) 
  | OrPat(_, p1, p2), v ->
    begin match match_pat_opt p1 v with
    | Some(t) -> Some(t)
    | None -> match_pat_opt p2 v
    end
  | DataPat(_, constructor_name, pattern), DataConV(val_constructor_name, underlying) when constructor_name = val_constructor_name ->
    match_pat_opt pattern underlying
  | _ -> None

let match_pat pat scrut locs =
  match match_pat_opt pat scrut with
  | None -> raise (EvalError.NonExhaustiveMatch (scrut, locs))
  | Some trans -> trans

let rec match_params patterns arg_vals locs = match patterns, arg_vals with
| [], [] -> fun x -> x
| ([] as pats), (_ as args) | (_ as pats), ([] as args) -> raise (EvalError.InvalidNumberOfArguments (pats, args, locs))
| pat :: pats, arg :: args -> 
  let trans = match_pat pat arg locs in
  fun s -> match_params pats args locs (trans s)

let rec eval_mod_expr env = function
| Import ((loc, _, body), _) -> 
    (* TODO This ignores the header for now. I hope this is fine? *)
    let _, module_env = eval_seq_state (empty_eval_env env.argv) body in
    { modules = module_env.module_vars; 
      mod_vars = module_env.vars;
    }
| ModVar (loc, var) -> 
  begin match VarMap.find_opt var env.module_vars with
  | None -> panic __LOC__ (Loc.pretty loc ^ ": Module variable not found at runtime: '" ^ Name.pretty var ^ "'. This should have been caught way earlier!")
  | Some runtime_module -> runtime_module
  end
| SubModule (loc, mod_expr, name) -> 
  let runtime_module = eval_mod_expr env mod_expr in
  begin match VarMap.find_opt name runtime_module.modules with
  | None -> panic __LOC__ (Loc.pretty loc ^ ": Submodule not found at runtime: '" ^ Name.pretty name ^ "'. This should have been caught way earlier!")
  | Some runtime_module -> runtime_module
  end

and eval_expr (env : eval_env) (expr : Renamed.expr) : value =
  let open Renamed in
  match expr with
  | Var (loc, x) ->
      if x.index = Name.primop_index
      then PrimOpV x.name
      else !(lookup_var env loc x)
  | DataConstructor (loc, name) ->
    PartialDataConV(name)
  | ModSubscriptDataCon (void, _, _, _) ->
    absurd void
  | App (loc, f, args) ->
    (* See Note [left-to-right evaluation] *)
    let f_val = eval_expr env f in
    let arg_vals = List.map (eval_expr env) args in
    eval_app env loc f_val arg_vals
  | Lambda (_, patterns, e) -> ClosureV (lazy env, patterns, e)
  
  | StringLit (_, s) -> StringV s
  | NumLit (_, f)    -> NumV f
  | BoolLit (_, b)   -> BoolV b
  | UnitLit _        -> unitV
  | NullLit _        -> NullV

  | ListLit (_, exprs) -> 
    let vals = List.map (eval_expr env) exprs in 
    ListV vals
  | TupleLit (_, exprs) ->
    let vals = Array.map (eval_expr env) (Array.of_list exprs) in
    TupleV vals
  | RecordLit (_, kvs) ->
    let kv_vals = List.map (fun (k, e) -> (k, eval_expr env e)) kvs in
    RecordV (RecordVImpl.of_list kv_vals)

  | Subscript (loc, map_expr, key) ->
    begin match eval_expr env map_expr with
    | RecordV map -> 
      begin match RecordVImpl.find key map with
      | (value :: _) -> value
      | [] -> raise (EvalError.MapDoesNotContain (map, key, loc :: env.call_trace))
      end
    | value -> raise (EvalError.TryingToLookupInNonMap (value, key, loc :: env.call_trace))
    end
  | ModSubscript (loc, mod_name, name) ->
    let runtime_module = match VarMap.find_opt mod_name env.module_vars with
      | None -> panic __LOC__ (Loc.pretty loc ^ ": Module variable not found at runtime: '" ^ Name.pretty mod_name ^ "'. This should have been caught earlier!")
      | Some runtime_module -> runtime_module
    in
    begin match VarMap.find_opt name runtime_module.mod_vars with
    | None -> panic __LOC__ (Loc.pretty loc ^ ": Module member not found at runtime: '" ^ Name.pretty name ^ "'. This should have been caught earlier!")
    | Some reference -> !reference
    end
  | RecordUpdate (loc, expr, update_exprs) -> 
    begin match eval_expr env expr with
    | RecordV vals -> 
      (* See note [Left to Right evaluation] *)
      let update_vals = List.map (fun (k, expr) -> (k, eval_expr env expr)) update_exprs in
      let add_val (k, v) m = RecordVImpl.update k (function 
        | (_::vs) -> v::vs
        | [] -> panic __LOC__ "Empty value list in record update"
      ) m 
      in
      RecordV (List.fold_right add_val update_vals vals)
    | value -> panic __LOC__ ("Non-record value in record update: " ^ Value.pretty value)
    end
  | RecordExtension (loc, expr, ext_exprs) -> 
    begin match eval_expr env expr with
    | RecordV vals ->
      (* See note [Left to Right evaluation] *)
      let update_vals = List.map (fun (k, expr) -> (k, eval_expr env expr)) ext_exprs in
      RecordV (RecordVImpl.add_list update_vals vals)
    | value -> panic __LOC__ ("Non-record value in record update: " ^ Value.pretty value)
    end
  | DynLookup (loc, map_expr, key_expr) ->
    begin match eval_expr env map_expr with
    | RecordV map -> 
      begin match eval_expr env key_expr with
      | StringV key -> begin match RecordVImpl.find key map with
                        | value :: _ -> value
                        | [] -> NullV
                        end
      | value -> raise (EvalError.InvalidMapKey (value, map, loc :: env.call_trace))
      end
    | ListV list ->
      begin match eval_expr env key_expr with
      | NumV num when Float.is_integer num ->
        let index = Float.to_int num in
        begin match List.nth_opt list index with
        | Some x -> x
        | None -> raise (EvalError.IndexOutOfRange(list, index, loc :: env.call_trace))
        end
      | value -> raise (EvalError.InvalidListKey (value, list, loc :: env.call_trace))
      end
    | value -> raise (EvalError.TryingToLookupDynamicInNonMap (value, loc :: env.call_trace))
    end

  | BinOp (loc, e1, Add, e2) -> 
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    let context = "Trying to add " ^ Value.pretty v1 ^ " and " ^ Value.pretty v2 in
    NumV (as_num context (loc :: env.call_trace) v1 +. as_num context (loc :: env.call_trace) v2)
  | BinOp (loc, e1, Sub, e2) -> 
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    let context = "Trying to subtract " ^ Value.pretty v2 ^ " from " ^ Value.pretty v1 in
    NumV (as_num context (loc :: env.call_trace) v1 -. as_num context (loc :: env.call_trace) v2)
  | BinOp (loc, e1, Mul, e2) -> 
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    let context = "Trying to multiply " ^ Value.pretty v1 ^ " and " ^ Value.pretty v2 in
    NumV (as_num context (loc :: env.call_trace) v1 *. as_num context (loc :: env.call_trace) v2)
  | BinOp (loc, e1, Div, e2) -> 
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    let context = "Trying to divide " ^ Value.pretty v1 ^ " by " ^ Value.pretty v2 in
    NumV (as_num context (loc :: env.call_trace) v1 /. as_num context (loc :: env.call_trace) v2)
  | BinOp (loc, e1, Cons, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    begin match v2 with
    | ListV values -> ListV (v1 :: values)
    | _ -> raise (EvalError.InvalidOperatorArgs("::", [v1; v2], loc :: env.call_trace))
    end

  | BinOp (loc, e1, Concat, e2) ->
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    begin match v1, v2 with
    | ListV xs, ListV ys -> ListV (xs @ ys)
    | RecordV xs, RecordV ys -> RecordV (RecordVImpl.union xs ys)
    | StringV s1, StringV s2 -> StringV (s1 ^ s2)
    | StringV s1, NumV _ -> StringV(s1 ^ Value.pretty v2)
    | NumV _, StringV s2 -> StringV(Value.pretty v1 ^ s2)
    | _, _ -> raise (EvalError.InvalidOperatorArgs("~", [v1; v2], loc :: env.call_trace))
    end 

  | BinOp (_, e1, Equals, e2) -> 
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    BoolV (val_eq v1 v2)
  | BinOp (_, e1, NotEquals, e2) -> 
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      BoolV (not (val_eq v1 v2))

  | BinOp (loc, e1, LE, e2) -> 
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    let context = "Trying to compute " ^ Value.pretty v1 ^ " <= " ^ Value.pretty v2 in
    BoolV (as_num context (loc :: env.call_trace) v1 <= as_num context (loc :: env.call_trace) v2)
  | BinOp (loc, e1, GE, e2) -> 
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    let context = "Trying to compute " ^ Value.pretty v1 ^ " >= " ^ Value.pretty v2 in
    BoolV (as_num context (loc :: env.call_trace) v1 >= as_num context (loc :: env.call_trace) v2)
  | BinOp (loc, e1, LT, e2) -> 
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    let context = "Trying to compute " ^ Value.pretty v1 ^ " < " ^ Value.pretty v2 in
    BoolV (as_num context (loc :: env.call_trace) v1 < as_num context (loc :: env.call_trace) v2)
  | BinOp (loc, e1, GT, e2) -> 
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    let context = "Trying to compute " ^ Value.pretty v1 ^ " > " ^ Value.pretty v2 in
    BoolV (as_num context (loc :: env.call_trace) v1 > as_num context (loc :: env.call_trace) v2)
  
  | BinOp (loc, e1, Or, e2) ->
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
  | BinOp (loc, e1, And, e2) ->
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
  | LetSeq _ | LetRecSeq _ | LetEnvSeq _ | LetModuleSeq _ | LetDataSeq _ -> raise (Panic "let assignment found outside of sequence expression")

  | Let (loc, pat, e1, e2) ->
    let scrut = eval_expr env e1 in
    let env_trans =match_pat pat scrut (loc :: env.call_trace) in
    eval_expr (env_trans env) e2

    (* We can safely ignore the type annotation since it has been checked by the
       typechecker already *)
  | LetRec (loc, _ty, f, params, e1, e2) ->
    let rec env' = lazy (fst (insert_var f (ClosureV (env', params, e1)) env)) in
    eval_expr (Lazy.force env') e2
  | LetEnv (loc, x, e1, e2) ->
    let env' = insert_env_var loc x (eval_expr env e1) env in
    eval_expr env' e2
  | Assign (loc, x, e1) ->
    let x_ref = lookup_var env loc x  in
    x_ref := (eval_expr env e1);
    unitV

  | ProgCall (loc, prog, args) as expr -> 
    eval_expr env (Pipe (loc, [expr]))

  | Pipe (loc, []) -> raise (Panic "empty pipe") 

  | Pipe (loc, ((ProgCall _ :: _) as exprs)) -> 
    let progs = progs_of_exprs env exprs in
    let in_chan, pid = Pipe.compose_in (Some (full_env_vars env)) progs in
    let result = StringV (trim_output (In_channel.input_all in_chan)) in
    env.last_status := Pipe.wait_to_status pid;
    result

  | Pipe (loc, (expr :: exprs)) ->
    let output_lines = Value.as_args (fun x -> raise (EvalError.InvalidProcessArg (x, loc :: env.call_trace))) (eval_expr env expr) in

    let progs = progs_of_exprs env exprs in

    let in_chan, pid = Pipe.compose_in_out (Some (full_env_vars env)) progs (fun out_chan ->
        List.iter (fun str -> Out_channel.output_string out_chan (str ^ "\n")) output_lines
      ) in
    let result = StringV (trim_output (In_channel.input_all in_chan)) in
    env.last_status := Pipe.wait_to_status pid;
    result
  | EnvVar(loc, var) ->
    (* We first check if the env var has been locally overriden by a 
        'let $x = ...' expression. If it has not, we look for actual environment variables *)
    begin match EnvMap.find_opt var env.env_vars with
    | Some(str) -> StringV str
    | None -> begin match Sys.getenv_opt var with
      | None -> NullV
      | Some(str) -> StringV str
      end
    end
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
        begin match match_pat_opt pat scrut_val with
        | Some env_trans ->
          eval_expr (env_trans env) expr
        | None -> go branches
        end
      | [] -> raise (EvalError.NonExhaustiveMatch (scrut_val, loc :: env.call_trace))
    in
    go branches
  | Ascription (_, expr, _) ->
    (* Ascriptions don't affect the evaluator. 
       These have been checked by the type checker already *)
    eval_expr env expr
  | Unwrap (loc, expr) ->
    let value = eval_expr env expr in
    begin match value with
    | DataConV (_, underlying) -> underlying
    | _ -> panic __LOC__ (Loc.pretty loc ^ ": Trying to unwrap non data constructor value: " ^ Value.pretty value)
    end

and eval_app env loc fun_v arg_vals = 
  match fun_v with
  | ClosureV (clos_env, params, body) ->
      (* Function arguments are evaluated left to right *)
      let env_trans = match_params params arg_vals (loc :: env.call_trace) in

      let updated_clos_env = env_trans (Lazy.force clos_env) in
      (* The call trace is extended and carried over to the closure environment *)
      eval_expr {updated_clos_env with call_trace = loc :: env.call_trace} body
  | PartialDataConV (constructor_name) ->
    begin match arg_vals with
    | [x] -> DataConV(constructor_name, x)
    | _ -> panic __LOC__ ("Invalid number of arguments for data constructor '" ^ Name.pretty constructor_name ^ "': [" ^ String.concat ", " (List.map Value.pretty arg_vals) ^ "]")
    end
  | PrimOpV prim_name ->
      eval_primop {env with call_trace = loc :: env.call_trace} prim_name arg_vals loc
  | x ->
      raise (EvalError.TryingToApplyNonFunction (x, loc :: env.call_trace))



(* This takes a continuation argument in order to stay mutually tail recursive with eval_expr *)
and eval_seq_cont : 'r. eval_env -> Renamed.expr list -> (eval_env -> (Renamed.expr, value) either -> 'r) -> 'r =
  fun env exprs cont ->
  match exprs with
  | [] -> cont env (Right unitV)
  | LetSeq (loc, pat, e) :: exprs -> 
    let scrut = eval_expr env e in
    let env_trans = match_pat pat scrut (loc :: env.call_trace) in
    eval_seq_cont (env_trans env) exprs cont

    (* We can safely ignore the type annotation since it has been checked by the typechecker already *)
  | LetRecSeq (loc, _ty, f, params, e) :: exprs ->
    let rec env' = lazy (fst (insert_var f (ClosureV (env', params, e)) env)) in
    eval_seq_cont (Lazy.force env') exprs cont
  | LetEnvSeq (loc, x, e) :: exprs ->
    let env' = insert_env_var loc x (eval_expr env e) env in
    eval_seq_cont env' exprs cont
  | LetModuleSeq (loc, x, me) :: exprs ->
    let module_val = eval_mod_expr env me in
    let env = insert_module_var x module_val env in
    eval_seq_cont env exprs cont
  | LetDataSeq (loc, _, _, _) :: exprs -> 
    (* Types are erased at runtime, so we don't need to do anything clever here *)
    eval_seq_cont env exprs cont
  (* Single program calls are just evaluated like pipes *)
  | ProgCall (loc, _, _) as expr :: exprs ->
    eval_seq_cont env (Pipe (loc, [expr]) :: exprs) cont
  
  (* Pipes in seq exprs inherit the parents stdout. *)
  (* Pipes without value inputs also inherit the parents stdin *)
  | Pipe (loc, ((ProgCall _ :: _) as prog_exprs)) :: exprs -> 
    let progs = progs_of_exprs env prog_exprs in
    let pid = Pipe.compose (Some (full_env_vars env)) progs in
    env.last_status := Pipe.wait_to_status pid;
    eval_seq_cont env exprs cont
  | Pipe (loc, (expr :: prog_exprs)) :: exprs ->
    let output_lines = Value.as_args (fun x -> raise (EvalError.InvalidProcessArg (x, loc :: env.call_trace))) (eval_expr env expr) in
    
    let progs = progs_of_exprs env prog_exprs in
    
    let pid = Pipe.compose_out_with (Some (full_env_vars env)) progs (fun out_chan -> 
        List.iter (fun line -> Out_channel.output_string out_chan (line ^ "\n")) output_lines
      ) in
    env.last_status := Pipe.wait_to_status pid;
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

and eval_list_comp env loc result_expr = function
  | Renamed.FilterClause expr :: comps -> 
    begin match eval_expr env expr with
    | BoolV false -> []
    | BoolV true -> eval_list_comp env loc result_expr comps
    | v -> raise (EvalError.NonBoolInListComp (v, loc :: env.call_trace))
    end
  | DrawClause (pattern, expr) :: comps ->
    begin match eval_expr env expr with
    | ListV values ->
      let eval_with_val v =
        match match_pat_opt pattern v with
        | None -> []
        | Some env_trans -> 
          eval_list_comp (env_trans env) loc result_expr comps
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
    unitV
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
  | "lines" -> begin match args with
              | [StringV ""] -> ListV []
              | [StringV arg] -> 
                ListV (List.map (fun s -> StringV s) (String.split_on_char '\n' arg))
              | _ -> raise (PrimOpArgumentError ("lines", args, "Expected a single string", loc :: env.call_trace))
              end
  | "split" -> begin match args with
              | [_; StringV ""] -> ListV []
              | [StringV sep_str; StringV arg] when String.length sep_str = 1 ->
                let sep = String.get sep_str 0 in
                ListV (List.map (fun s -> StringV s) (String.split_on_char sep arg))
              | _ -> raise (PrimOpArgumentError ("split", args, "Expected (char, string)", loc :: env.call_trace))
              end
  | "replace" -> begin match args with
                | [StringV needle; StringV repl; StringV str_v] -> 
                  StringV (Re.replace_string (Re.compile (Re.str needle)) ~by:repl str_v)
                | _ -> raise (PrimOpArgumentError ("replace", args, "Expected three strings", loc :: env.call_trace))
                end
  | "regexpReplace" -> begin match args with
                | [StringV pattern; StringV repl; StringV str] -> 
                  let regexp = Re.Pcre.regexp pattern in
                  StringV (Re.replace_string regexp ~by:repl str)
                | _ -> raise (PrimOpArgumentError ("regexpReplace", args, "Expected three strings", loc :: env.call_trace))
                end
  | "regexpMatch" -> begin match args with
                | [StringV pattern; StringV arg] ->
                  let regexp = Re.Pcre.regexp pattern in
                  begin try
                    let results = Re.matches regexp arg in
                    ListV (List.map (fun x -> StringV (x)) results)
                  with
                  | Not_found -> ListV []
                  end

                | _ -> raise (PrimOpArgumentError ("regexpMatch", args, "Expected (string, string)", loc :: env.call_trace))
                end
  | "regexpTransform" -> begin match args with
                | [StringV pattern; transformClos; StringV str_v] ->
                    let regexp = Re.Pcre.regexp pattern in

                    let transform group = 
                      match eval_app env loc transformClos [StringV (Re.Group.get group 0)] with
                      | StringV repl -> repl
                      | value -> raise (PrimOpError ("regexpTransform", "Replacement function did not return a string. Returned value: " ^ Value.pretty value, loc :: env.call_trace))
                    in

                    StringV (Re.replace regexp ~f:transform str_v)
                | _ -> raise (PrimOpArgumentError ("regexpTransform", args, "Expected (string, function, string)", loc :: env.call_trace))
                end
  | "regexpTransformAll" -> begin match args with
                | [StringV pattern; transformClos; StringV str_v] ->
                  let regexp = Re.Pcre.regexp pattern in

                  let transform group = 
                    match eval_app env loc transformClos [ListV (List.map (fun x -> StringV x) (Array.to_list (Re.Group.all group)))] with
                    | StringV repl -> repl
                    | value -> raise (PrimOpError ("regexpTransform", "Replacement function did not return a string. Returned value: " ^ Value.pretty value, loc :: env.call_trace))
                  in

                  StringV (Re.replace regexp ~f:transform str_v)
                | _ -> raise (PrimOpArgumentError ("regexpTransformAll", args, "Expected (string, function, string)", loc :: env.call_trace))
                end
  | "writeFile" -> begin match args with
                | [path_v; content_v] ->
                  let context = "Trying to apply 'writeFile'" in
                  let path = as_string context (loc :: env.call_trace) path_v in
                  let content = as_string context (loc :: env.call_trace) content_v in

                  let channel = open_out path in
                  Out_channel.output_string channel content;
                  Out_channel.close channel;
                  unitV
                | _ -> raise (PrimOpArgumentError ("writeFile", args, "Expected two string arguments", loc :: env.call_trace))
                end
  | "parseInt" -> begin match args with
                | [StringV arg_str] ->
                  begin match int_of_string_opt (String.trim arg_str) with
                  |  Some(int_val) -> NumV (float_of_int int_val)
                  |  None -> raise (PrimOpArgumentError ("parseInt", args, "Expected a string containing an integer literal", loc :: env.call_trace))
                  end
                | _ -> raise (PrimOpArgumentError ("parseInt", args, "Expected a string", loc :: env.call_trace))
                end
  | "parseNum" -> begin match args with
                | [StringV arg_str] ->
                  begin match float_of_string_opt (String.trim arg_str) with
                  | Some(float_val) -> NumV float_val
                  | None -> raise (PrimOpArgumentError ("parseNum", args, "Expected a string containing a numeric literal", loc :: env.call_trace))
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
                  unitV
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
  | "insert" -> todo __LOC__
  | "mapToList" -> todo __LOC__
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
                  unitV
                else
                  raise (EnsureFailed (path, loc::env.call_trace))
              | _ -> raise (PrimOpArgumentError("ensure", args, "Expected a string", loc :: env.call_trace))
              end
  | "status" -> NumV (Float.of_int !(env.last_status))
  | _ -> raise (Panic ("Invalid or unsupported primop: " ^ op))

and progs_of_exprs env = function
  | ProgCall (loc, progName, args) :: exprs ->
    let fail x = raise (EvalError.InvalidProcessArg (x, loc :: env.call_trace)) in
    let arg_strings = List.concat_map (fun arg -> Value.as_args fail (eval_expr env arg)) args in
    (progName, arg_strings) :: progs_of_exprs env exprs
  | expr :: exprs -> 
    raise (EvalError.NonProgCallInPipe (expr, Renamed.get_loc expr :: env.call_trace))
  | [] -> []

and empty_eval_env (argv: string list): eval_env = {
  vars = VarMap.empty;
  env_vars = EnvMap.empty;
  argv = argv;
  call_trace = [];
  last_status = ref 0;
  module_vars = VarMap.empty
}

let eval (argv : string list) (exprs : Renamed.expr list) : value = eval_seq (empty_eval_env argv) exprs

let flag_info_of_flag_def (env : eval_env) (flag_def : Renamed.flag_def): Argparse.flag_info * eval_env =
  let aliases = flag_def.flags in
  let description = Option.value ~default:"" flag_def.description in
  match flag_def.args with
  | Switch name ->
    let env, flag_ref = insert_var name (BoolV false) env in
    { aliases
    ; arg_count = 0
    ; required = false
    ; description
    ; action = fun _ -> flag_ref := BoolV true
    }, env
  | Varargs name ->
    let env, flag_ref = insert_var name (ListV []) env in
    { aliases
    ; arg_count = 1
    ; required = false
    ; description
    ; action = fun args ->
      match !flag_ref with
      | ListV prev_args -> flag_ref := ListV (prev_args @ (List.map (fun x -> StringV x) args))
      | v -> raise (Panic ("flag_info_of_flag_def: Non-list value in varargs flag reference: " ^ Value.pretty v))
    }, env
  | Named args ->
    let env, refs = List.fold_left_map (
      fun env x -> insert_var x NullV env
      ) env args
    in
    { aliases
    ; arg_count = List.length args
    ; required = true
    ; description
    ; action = fun args ->
      List.iter2 (fun r x -> r := StringV x) refs args
    }, env  
  | NamedDefault args ->
    let env, refs = List.fold_left_map (
      fun env (x, default) -> insert_var x (StringV default) env
      ) env args
    in
    { aliases
    ; arg_count = List.length args
    ; required = false
    ; description
    ; action = fun args ->
      List.iter2 (fun r x -> r := StringV x) refs args
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

  let args = Argparse.run infos prog_info (fun msg -> raise (EvalError.ArgParseError msg)) (List.tl env.argv) in

  { env with argv = (List.hd env.argv :: args) }


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
