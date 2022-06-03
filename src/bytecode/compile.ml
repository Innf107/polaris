open Util
open Syntax
open Bytecode

module VarMap = Map.Make (Name)
module StringMap = Map.Make (String)

type expr = Renamed.expr

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

let emit_function_with (make_block : int -> function_block) (state : compile_state) : int =
  let index = state.function_count in
  state.function_count <- state.function_count + 1;
  state.functions <- state.functions @ [make_block index]; (* Ugh, this really shouldn't be quadratic... *)
  index
  
let emit_function (block : function_block) state = emit_function_with (fun _ -> block) state

let insert_new_var (var : name) (env : compile_env) : int * compile_env =
  let index = env.local_count in
  let new_env = { env with
    locals = VarMap.add var index env.locals
  ; local_count = env.local_count + 1;
  } in
  index, new_env

let add_params (params : name list) (env : compile_env) : compile_env =
  let param_count = List.length params in
  {
    locals = VarMap.add_seq (List.to_seq (List.mapi (fun i x -> (x, i)) params)) 
              (VarMap.map (fun x -> x + param_count) env.locals)
  ; local_count = env.local_count + param_count
  ; state = env.state
  }

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
    let body_ops = compile_expr (add_params params env) body in
    let fun_index = emit_function { body = body_ops; arg_count = List.length params } env.state in
    [Closure fun_index]

  | StringLit (loc, lit) -> [StringLit (emit_string lit env.state)]

  | NumLit (loc, lit) -> [FloatLit lit]

  | UnitLit loc -> [UnitLit]

  | ListLit (loc, exprs) ->
    let expr_ops = List.concat_map (compile_expr env) exprs in
    expr_ops @ [AllocList (List.length expr_ops)]

  | Let (loc, x, body, e) -> 
    compile_let env loc x body (fun new_env -> compile_expr new_env e)

  | LetRec (loc, x, params, body, e) ->
    compile_let_rec env loc x params body (fun new_env -> compile_expr new_env e)
  | _ -> todo __POS__

and compile_expr_seq (env : compile_env) (exprs : expr list) : op list =
  match exprs with
  | [] -> []
  | LetSeq (loc, x, body) :: exprs ->
    compile_let env loc x body (fun new_env -> compile_expr_seq new_env exprs)
  | LetRecSeq (loc, x, params, body) :: exprs ->
    compile_let_rec env loc x params body (fun new_env -> compile_expr_seq new_env exprs)
  | expr :: exprs ->
    let expr_bytecode = compile_expr env expr in
    let exprs_bytecode = compile_expr_seq env exprs in
    expr_bytecode @ exprs_bytecode

and compile_let (env : compile_env) (loc : loc) (x : name) (body : expr) (cont : compile_env -> op list) : op list =
  let body_bytecode = compile_expr env body in
  let (ix, new_env) = insert_new_var x env in
  body_bytecode @ [StoreLocal ix] @ cont new_env

and compile_let_rec (env : compile_env) (loc : loc) (x : name) (params : name list) (body : expr) (cont : compile_env -> op list) : op list =
  let param_count = List.length params in
  (* Recursive bindings reserve an additional local slot for their closure. *)
  let body_env = add_params (params @[x]) env in

  let body_bytecode = compile_expr body_env body in

  let fun_index = emit_function_with (fun fun_index -> { 
    (* We store the recursive closure one cell after the function parameters. *)
    body = [Closure fun_index; StoreLocal param_count] @ body_bytecode; 
    arg_count = List.length params 
  }) env.state in

  let (ix, new_env) = insert_new_var x env in
  body_bytecode @ [Closure fun_index; StoreLocal ix] @ cont new_env


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


