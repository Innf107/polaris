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
  | UnitV (* TODO: Unit expression *)
  | BoolV of bool

module EvalError = struct
  exception DynamicVarNotFound of name
  exception UnableToConvertTo of string * value * string
  exception TryingToApplyNonFunction of value
  exception InvalidNumberOfArguments of name list * value list
end  

module Value = struct
  type t = value

  let pretty (x : t) : string =
    match x with
    | Untyped s -> s
    | StringV s -> s
    | NumV n -> string_of_float n
    | ClosureV (_, params, _) ->
        "<closure(" ^ String.concat ", " (List.map Name.pretty params) ^ ")>"
    | UnitV -> "()"
    | BoolV b -> string_of_bool b
end

(* TODO: handle non-existant variables more gracefully *)
let lookup_var (env : eval_env) (var : name) : value ref = 
  try 
    VarMap.find var env.vars
  with
    Not_found -> raise (EvalError.DynamicVarNotFound var)

let insert_vars (vars : name list) (vals : value list) (env : eval_env): eval_env =
  if (List.compare_lengths vars vals != 0) 
  then raise (EvalError.InvalidNumberOfArguments (vars, vals))
  else { vars = List.fold_right2 (fun x v r -> VarMap.add x (ref v) r) vars vals env.vars }

(* Tries to convert a number to a value. Throws an error if it can't *)
let as_num context = function
  | NumV x -> x
  | Untyped value ->
    begin match float_of_string_opt value with
    | Some x -> x
    | None -> raise (EvalError.UnableToConvertTo ("number", Untyped value, context))
    end
  | x -> raise (EvalError.UnableToConvertTo ("number", x, context))

let as_bool context = function
  | BoolV x -> x
  | Untyped value ->
    begin match bool_of_string_opt value with
    | Some x -> x
    | None -> raise (EvalError.UnableToConvertTo ("boolean", Untyped value, context))
    end
  | x -> raise (EvalError.UnableToConvertTo ("boolean", x, context))

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
  | Var x -> !(lookup_var env x)
  | App (f, args) -> (
      match eval_expr env f with
      | ClosureV (clos_env, params, body) ->
          (* Function arguments are evaluated left to right *)
          let arg_vals = List.map (eval_expr env) args in
          eval_expr (insert_vars params arg_vals (Lazy.force clos_env)) body
      | x ->
          raise (EvalError.TryingToApplyNonFunction x) (* TODO: "Trying to apply non-function value 'x'" *))
  | Lambda (params, e) -> ClosureV (lazy env, params, e)
  
  | StringLit s -> StringV s
  | NumLit f    -> NumV f
  | UnitLit     -> UnitV
  
  (* TODO: Handle untyped *)
  | Add (e1, e2) -> 
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    let context = "Trying to add " ^ Value.pretty v1 ^ " and " ^ Value.pretty v2 in
    NumV (as_num context v1 +. as_num context v2)
  | Sub (e1, e2) -> 
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    let context = "Trying to subtract " ^ Value.pretty v2 ^ " from " ^ Value.pretty v1 in
    NumV (as_num context v1 -. as_num context v2)
  | Mul (e1, e2) -> 
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    let context = "Trying to multiply " ^ Value.pretty v1 ^ " and " ^ Value.pretty v2 in
    NumV (as_num context v1 *. as_num context v2)
  | Div (e1, e2) -> 
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    let context = "Trying to divide " ^ Value.pretty v1 ^ " by " ^ Value.pretty v2 in
    NumV (as_num context v1 /. as_num context v2)

  | Equals (e1, e2) -> 
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    BoolV (val_eq v1 v2)

  | LE (e1, e2) -> 
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e2 in
    let v2 = eval_expr env e2 in
    let context = "Trying to compute " ^ Value.pretty v1 ^ " <= " ^ Value.pretty v2 in
    BoolV (as_num context v1 <= as_num context v2)
  | GE (e1, e2) -> 
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e2 in
    let v2 = eval_expr env e2 in
    let context = "Trying to compute " ^ Value.pretty v1 ^ " >= " ^ Value.pretty v2 in
    BoolV (as_num context v1 >= as_num context v2)
  | LT (e1, e2) -> 
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e2 in
    let v2 = eval_expr env e2 in
    let context = "Trying to compute " ^ Value.pretty v1 ^ " < " ^ Value.pretty v2 in
    BoolV (as_num context v1 < as_num context v2)
  | GT (e1, e2) -> 
    (* See Note [left-to-right evaluation] *)
    let v1 = eval_expr env e2 in
    let v2 = eval_expr env e2 in
    let context = "Trying to compute " ^ Value.pretty v1 ^ " > " ^ Value.pretty v2 in
    BoolV (as_num context v1 > as_num context v2)
  
  | If (e1, e2, e3) ->
    let v1 = eval_expr env e1 in
    let context = "" in
    if as_bool context v1 then 
      eval_expr env e2
    else
      eval_expr env e3

  | Seq exprs -> eval_seq env exprs
  | LetSeq _ | LetRecSeq _ -> raise (Panic "let assignment found outside of sequence expression")

  | Let (x, e1, e2) ->
    eval_expr (insert_vars [x] [eval_expr env e1] env) e2
  | LetRec (f, params, e1, e2) ->
    let rec env' = lazy (insert_vars [f] [ClosureV (env', params, e1)] env) in
    eval_expr (Lazy.force env') e2
  | Assign (x, e1) ->
    let x_ref = lookup_var env x in
    x_ref := (eval_expr env e1);
    UnitV
  | Print expr ->
    let value = eval_expr env expr in
    print_endline (Value.pretty value);
    UnitV
  | ProgCall (prog, args) -> raise TODO (* TODO *)
  | Pipe exprs -> raise TODO (* TODO *)

and eval_seq (env : eval_env) (exprs : name_expr list) : value =
  match exprs with
  | [] -> UnitV
  | LetSeq (x, e) :: exprs -> 
    eval_seq (insert_vars [x] [(eval_expr env e)] env) exprs
  | LetRecSeq (f, params, e) :: exprs ->
    let rec env' = lazy (insert_vars [f] [ClosureV (env', params, e)] env) in
    eval_seq (Lazy.force env') exprs
  | [ e ] -> eval_expr env e
  | e :: exprs ->
      (* The result of 'eval_expr e' is purposefully ignored *)
      let _ = eval_expr env e in
      eval_seq env exprs

let eval (expr : name_expr) : value = eval_expr { vars = VarMap.empty } expr


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
