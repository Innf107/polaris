open Util
open Ast
open Bytecode

module VarMap = Map.Make (Name)

type expr = NameExpr.expr

type compile_env = {
    vars : int VarMap.t
  }

let empty_compile_env = { vars = VarMap.empty }

let compile_expr (env : compile_env) (expr : expr) : program * function_block list =
  match expr with
  | NumLit (loc, lit) -> raise TODO
  | _ -> raise TODO

let compile (expr : expr) : Bytecode.program =
  let prog, _ = compile_expr empty_compile_env expr in
  prog


