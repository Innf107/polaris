open Util
open Ast
open Bytecode

module VarMap = Map.Make (Name)
module StringMap = Map.Make (String)

type expr = NameExpr.expr

type compile_state = {
    mutable functions : function_block list
  ; mutable function_count : int

  ; mutable strings : int StringMap.t
  ; mutable string_count : int
  }

type compile_env = {
    locals : int VarMap.t
  ; local_count : int
  ; state : compile_state
  }

let new_compile_state () = {
    functions = []
  ; function_count = 0

  ; strings = StringMap.empty
  ; string_count = 0
  }

let empty_compile_env (state : compile_state) = { locals = VarMap.empty; local_count = 0; state }

let emit_string (str : string) (state : compile_state) : int =
  let index = state.string_count in
  state.string_count <- state.string_count + 1;
  state.strings <- StringMap.add str index state.strings;
  index

let emit_function (block : function_block) (state : compile_state) : int =
  let index = state.function_count in
  state.function_count <- state.function_count + 1;
  state.functions <- state.functions @ [block]; (* Ugh, this really shouldn't be quadratic... *)
  index

let insert_new_var (var : name) (env : compile_env) : int * compile_env =
  let index = env.local_count in
  let new_env = { env with
    locals = VarMap.add var index env.locals
  ; local_count = env.local_count + 1;
  } in
  index, new_env

let rec compile_expr (env : compile_env) (expr : expr) : op list =
  match expr with
  | Var (loc, name) -> 
    begin match VarMap.find_opt name env.locals with
    | Some ix -> [LoadLocal ix]
    | None -> raise (Panic ("Unable to find variable '" ^ Name.pretty name ^ "' while generating bytecode at " ^ Loc.pretty loc))
    end
  | App (loc, f, args) ->
    let f_ops = compile_expr env f in
    let arg_ops = List.concat_map (compile_expr env) args in
    f_ops @ [StoreTemp 0] @ arg_ops @ [LoadTemp 0; CallDyn]
  | Lambda (loc, params, body) ->
    let param_count = List.length params in
    let body_env = List.fold_right (fun x env -> snd (insert_new_var x env)) params {
        locals = VarMap.map (fun x -> x + param_count) env.locals
      ; local_count = env.local_count + param_count
      ; state = env.state
      } in
    let body_ops = compile_expr body_env body in
    let fun_index = emit_function { body = body_ops; arg_count = param_count } env.state in
    [ClosureLit fun_index]
  | StringLit (loc, lit) -> [StringLit (emit_string lit env.state)]
  | NumLit (loc, lit) -> [FloatLit lit]
  | UnitLit loc -> [UnitLit]
  | ListLit (loc, exprs) ->
    let expr_ops = List.concat_map (compile_expr env) exprs in
    expr_ops @ [AllocList (List.length expr_ops)]
  | _ -> raise TODO
and compile_expr_seq (env : compile_env) (exprs : expr list) : op list =
  match exprs with
  | [] -> []
  | LetSeq (loc, x, e) :: exprs ->
    let expr_bytecode = compile_expr env e in
    let (ix, new_env) = insert_new_var x env in
    expr_bytecode @ [StoreLocal ix] @ compile_expr_seq new_env exprs
  | LetRecSeq _ :: exprs -> raise TODO
  | expr :: exprs -> compile_expr env expr @ compile_expr_seq env exprs

let extract_strings (state : compile_state) : string list =
  List.map fst (List.sort (fun (_, ix1) (_, ix2) -> compare ix1 ix2) (StringMap.bindings state.strings))

let compile (exprs : expr list) : Bytecode.program =
  let state = new_compile_state () in
  let env = empty_compile_env state in

  let ops = compile_expr_seq env exprs in
  { functions = state.functions
  ; main = ops
  ; strings = extract_strings state
  }


