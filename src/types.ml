open Syntax
open Renamed

open Util

module VarTypeMap = Map.Make(Name)

module Subst = struct
  type t = ty VarTypeMap.t

  let empty : t = VarTypeMap.empty

  let concat : t list -> t = 
    List.fold_left (VarTypeMap.union (fun _ _ y -> Some y)) VarTypeMap.empty

  let apply : t -> ty -> ty =
    raise TODO
end



type tc_env = {
  var_types : ty VarTypeMap.t
}

let lookup_var var env =
  match VarTypeMap.find_opt var env.var_types with
  | Some (ty) -> ty
  | None -> raise (Panic ("lookup_var: Variable not found during typechecking: " ^ Name.pretty var))

let decompose_fun loc = function
  | _ -> raise TODO

let instantiate = function
  | _ -> raise TODO

(* Ensure that `ty1` is a subtype of `ty2` (`ty1 ≤ ty2`)*)
let subtype loc ty1 ty2 = match ty1, ty2 with
  | _ -> raise TODO


let rec check : tc_env -> expr -> ty -> Subst.t =
  fun env expr ty -> 
  match expr with
  (* We only include cases that actually benefit from checking here
    (as opposed to inference and subtyping) *)
  | Var (loc, var_name) ->
    (* This is a special case, since inference has to instantiate the type and we don't want that when checkign*)
    let var_ty = lookup_var var_name env in
    subtype loc var_ty ty
  | expr ->
    let expr_ty = infer env expr in
    subtype (get_loc expr) expr_ty ty

and infer : tc_env -> expr -> ty * Subst.t =
  fun env expr ->
  match expr with
  | Var (loc, var_name) -> 
    (instantiate (lookup_var var_name env), Subst.empty)
  
  | App (loc, fun_expr, arg_exprs) ->
    let fun_ty = infer env fun_expr in
    let fun_dom, fun_codom = decompose_fun loc fun_ty in

    let subst = Subst.concat (List.map2 (fun arg_expr dom_ty -> check env arg_expr dom_ty) arg_exprs fun_dom) in

    (Subst.apply subst fun_codom, subst)

  | Lambda (loc, args, body) ->
    raise TODO

  | NumLit _    -> (Number, Subst.empty)
  | StringLit _ -> (String, Subst.empty)
  | BoolLit _   -> (Bool, Subst.empty)
  | _ -> raise TODO

and infer_seq env exprs = match exprs with
  | [] -> Unit, Subst.empty, env
  | expr::exprs ->
    let _ty, subst = infer env expr in
    (*TODO: apply subst*)
    infer_seq env exprs

let typecheck exprs = 
  let ty, _subst, _ = infer_seq { var_types = VarTypeMap.empty } exprs in
  ty
