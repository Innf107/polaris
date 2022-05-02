open Ast
open Util

module VarMap = Map.Make (Name)

type eval_env = { vars : value ref VarMap.t }

and value =
  | Untyped of string
  | StringV of string
  | NumV of float
  (* The closure environment has to be lazy to
     support recursive lets, since the definition of a recursive function has
     to be stored in its own closure, which also stores its on environment, etc.*)
  | ClosureV of eval_env lazy_t * name list * name_expr
  | UnitV
  | BoolV of bool
  (* Lists are *immutable*. 
     Otherwise we would have to deal with some kind of a 'place' system, which is
     anything but ideal. One should be able to approximate mutable lists
     by mutable references to immutable lists 99% of the time, so this is
     hopefully not going to be an issue. *)
  | ListV of value list 

module EvalError = struct
  exception DynamicVarNotFound of name * loc
  exception UnableToConvertTo of string * value * string * loc
  exception TryingToApplyNonFunction of value * loc
  exception InvalidNumberOfArguments of name list * value list * loc
end  

module Value = struct
  type t = value

  let rec pretty (x : t) : string =
    match x with
    | Untyped s -> s
    | StringV s -> s
    | NumV n -> 
      if Float.is_integer n
      then string_of_int (int_of_float n)
      else string_of_float n
    | ClosureV (_, params, _) ->
        "<closure(" ^ String.concat ", " (List.map Name.pretty params) ^ ")>"
    | UnitV -> "()"
    | BoolV b -> string_of_bool b
    | ListV vals -> "[" ^ String.concat ", " (List.map pretty vals) ^ "]"
end

let lookup_var (env : eval_env) (loc : loc) (var : name) : value ref = 
  try 
    VarMap.find var env.vars
  with
    Not_found -> raise (EvalError.DynamicVarNotFound (var, loc))

let insert_vars (vars : name list) (vals : value list) (env : eval_env) (loc : loc) : eval_env =
  if (List.compare_lengths vars vals != 0) 
  then raise (EvalError.InvalidNumberOfArguments (vars, vals, loc))
  else { vars = List.fold_right2 (fun x v r -> VarMap.add x (ref v) r) vars vals env.vars }

(* Tries to convert a number to a value. Throws an error if it can't *)
let as_num context loc = function
  | NumV x -> x
  | Untyped value ->
    begin match float_of_string_opt value with
    | Some x -> x
    | None -> raise (EvalError.UnableToConvertTo ("number", Untyped value, context, loc))
    end
  | x -> raise (EvalError.UnableToConvertTo ("number", x, context, loc))

let as_bool context loc = function
  | BoolV x -> x
  | Untyped value ->
    begin match bool_of_string_opt value with
    | Some x -> x
    | None -> raise (EvalError.UnableToConvertTo ("boolean", Untyped value, context, loc))
    end
  | x -> raise (EvalError.UnableToConvertTo ("boolean", x, context, loc))

let val_eq (x : value) (y : value) : bool = 
  match x, y with
  (* TODO: Handle untypeds *)
  | StringV x, StringV y -> x == y
  | NumV x, NumV y -> x == y
  (* Closure comparison always evaluates to false *)
  | (ClosureV _, _ | _, ClosureV _) -> false
  | UnitV, UnitV -> true
  | BoolV x, BoolV y -> x == y
  (* Comparisons of different (typed) values are always false *)
  | _, _ -> false

let rec eval_expr (env : eval_env) (expr : name_expr) : value =
  let open NameExpr in
  match expr with
  | Var (loc, x) -> !(lookup_var env loc x)
  | App (loc, f, args) -> (
      match eval_expr env f with
      | ClosureV (clos_env, params, body) ->
          (* Function arguments are evaluated left to right *)
          let arg_vals = List.map (eval_expr env) args in
          eval_expr (insert_vars params arg_vals (Lazy.force clos_env) loc) body
      | x ->
          raise (EvalError.TryingToApplyNonFunction (x, loc)))
  | Lambda (_, params, e) -> ClosureV (lazy env, params, e)
  
  | StringLit (_, s) -> StringV s
  | NumLit (_, f)    -> NumV f
  | UnitLit _        -> UnitV
  
  | ListLit (_, exprs) -> 
    let vals = List.map (eval_expr env) exprs in 
    ListV vals

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
  | ProgCall (loc, prog, args) -> raise TODO (* TODO *)
  | Pipe (loc, exprs) -> raise TODO (* TODO *)

and eval_seq_state (env : eval_env) (exprs : name_expr list) : value * eval_env =
  match exprs with
  | [] -> (UnitV, env)
  | LetSeq (loc, x, e) :: exprs -> 
    eval_seq_state (insert_vars [x] [(eval_expr env e)] env loc) exprs
  | LetRecSeq (loc, f, params, e) :: exprs ->
    let rec env' = lazy (insert_vars [f] [ClosureV (env', params, e)] env loc) in
    eval_seq_state (Lazy.force env') exprs
  | [ e ] -> (eval_expr env e, env)
  | e :: exprs ->
      (* The result of 'eval_expr e' is purposefully ignored *)
      let _ = eval_expr env e in
      eval_seq_state env exprs
and eval_seq env exprs = let (v, _) = eval_seq_state env exprs in v

let empty_eval_env : eval_env = {
  vars = VarMap.empty
}

let eval (exprs : name_expr list) : value = eval_seq empty_eval_env exprs


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
