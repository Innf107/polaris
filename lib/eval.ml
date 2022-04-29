open Ast
open Util

module EvalError = struct
  exception DynamicVarNotFound of name
end

module VarMap = Map.Make (Name)

type eval_env = { vars : value ref VarMap.t }

and value =
  | Untyped of string
  | StringV of string
  | IntV of int
  | FloatV of float
  | ClosureV of eval_env * name list * name_expr
  | UnitV (* TODO: Unit expression *)

module Value = struct
  type t = value

  let pretty (x : t) : string =
    match x with
    | Untyped s -> s
    | StringV s -> s
    | IntV n -> string_of_int n
    | FloatV f -> string_of_float f
    | ClosureV (_, params, _) ->
        "<closure(" ^ String.concat ", " (List.map Name.pretty params) ^ ")>"
    | UnitV -> "()"

end

(* TODO: handle non-existant variables more gracefully *)
let lookup_var (env : eval_env) (var : name) : value ref = 
  try 
    VarMap.find var env.vars
  with
    Not_found -> raise (EvalError.DynamicVarNotFound var)

let insert_vars (vars : name list) (vals : value list) (env : eval_env): eval_env =
  if (List.compare_lengths vars vals != 0) 
  then raise (Panic "trying to insert variables and values with different lengths")
  else { vars = List.fold_right2 (fun x v r -> VarMap.add x (ref v) r) vars vals env.vars }

let rec eval_expr (env : eval_env) (expr : name_expr): value =
  let open NameExpr in
  match expr with
  | Var x -> !(lookup_var env x)
  | App (f, args) -> (
      match eval_expr env f with
      | ClosureV (clos_env, params, body) ->
          (* Function arguments are evaluated left to right *)
          let arg_vals = List.map (eval_expr env) args in
          eval_expr (insert_vars params arg_vals clos_env) body
      | x ->
          raise TODO (* TODO: "Trying to apply non-function value 'x'" *))
  | Lambda (params, e) -> ClosureV (env, params, e)
  
  | StringLit s -> StringV s
  | IntLit n    -> IntV n
  | FloatLit f  -> FloatV f
  | UnitLit     -> UnitV
  
  | Seq exprs -> eval_seq env exprs
  | LetSeq (x, e) -> raise TODO (* TODO: "let assignment found outside of sequence expression" *)

  | Let (x, e1, e2) ->
    eval_expr (insert_vars [x] [eval_expr env e1] env) e2
  | Assign (x, e1) ->
    let x_ref = lookup_var env x in
    x_ref := (eval_expr env e1);
    UnitV
  | ProgCall (prog, args) -> raise TODO (* TODO *)
  | Pipe exprs -> raise TODO (* TODO *)

and eval_seq (env : eval_env) (exprs : name_expr list) : value =
  match exprs with
  | [] -> UnitV
  | LetSeq (x, e) :: exprs -> 
    eval_seq (insert_vars [x] [(eval_expr env e)] env) exprs
  | [ e ] -> eval_expr env e
  | e :: exprs ->
      (* The result of 'eval_expr e' is purposefully ignored *)
      let _ = eval_expr env e in
      eval_seq env exprs

let eval (expr : name_expr) : value = eval_expr { vars = VarMap.empty } expr
