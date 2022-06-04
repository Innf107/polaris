open Syntax
open Renamed

open Util

module VarTypeMap = Map.Make(Name)

type type_error =
  | NotASubtype of loc * ty * ty 

exception TypeError of type_error

let type_error err = raise (TypeError err)

module Subst = struct
  type t = T
  let empty : t = T

  let (<>) : t -> t -> t = fun _ _ -> T

  let flatten : t list -> t = fun _ -> T
end

type tc_env = {
  var_types : ty VarTypeMap.t
}

let lookup_var var env =
  match VarTypeMap.find_opt var env.var_types with
  | Some (ty) -> ty
  | None -> raise (Panic ("lookup_var: Variable not found during typechecking: " ^ Name.pretty var))

let instantiate = function
| Forall (tvs, ty) -> todo __POS__  
| _ -> todo __POS__

let skolemize = function
  | Forall (tvs, ty) -> todo __POS__
  | _ -> todo __POS__

(* Ensure that `ty1` is a subtype of `ty2` (`ty1 <: ty2`)*)
let rec subtype loc ty1 ty2 = let open Subst in match ty1, ty2 with
  | Number, Number | String, String | Bool, Bool | Unit, Unit -> Subst.empty
  | Skol (x, i), Skol (y, j) when x = y && i = j -> Subst.empty
  | ty1, (Forall _ as ty2) ->
    subtype loc ty1 (skolemize ty2)
  | (Forall _ as ty1), ty2 ->
    subtype loc (instantiate ty1) (skolemize ty2)
  | Fun (a_tys, b), Fun (c_tys, d) ->
    (* C <: A (Contravariance) *)
    Subst.flatten (List.map2 (subtype loc) c_tys a_tys) <>
    (* B <: D (Covariance) *)
    subtype loc b d
  | Any, _ -> Subst.empty (* TODO: Do we need to build a substitution here?*)
  | _, Any -> Subst.empty
  | Union (a, b), c ->
    subtype loc a c <>
    subtype loc b c
  | a, Union (b, c) -> begin
    try
      subtype loc a b
    with
    | TypeError _ -> subtype loc a c (* TODO: Add some context to the error message *)
    end
  | TupleList a_tys, TupleList b_tys ->
    Subst.flatten (List.map2 (subtype loc) a_tys b_tys)
  | TupleList a_tys, List b ->
    Subst.flatten (List.map (fun a -> subtype loc a b) a_tys)
  | List a, List b ->
    subtype loc a b
  (* TODO: Maps *)
  | _ -> type_error (NotASubtype (loc, ty1, ty2))

let app_subtype loc app_tys ty = match app_tys, ty with
  | _ -> todo __POS__

let rec check : tc_env -> expr -> ty -> Subst.t =
  fun env expr ty ->
  match expr with
  | Var (loc, x) ->
    let x_ty = lookup_var x env in
    subtype loc x_ty ty
  | NumLit (loc, _) ->
    subtype loc Number ty
  | _ -> todo __POS__

and infer : tc_env -> ty list -> expr -> ty * Subst.t =
  fun env app_tys expr ->
  match expr with
  | Var (loc, x) ->
    let x_ty = lookup_var x env in
    app_subtype loc app_tys x_ty
  | NumLit (loc, _) ->
    Number, Subst.empty
  | _ -> todo __POS__

and infer_seq env exprs = match exprs with
  | [] -> Unit, Subst.empty, env
  | expr::exprs ->
    todo __POS__

let typecheck exprs = 
  let ty, _subst, _ = infer_seq { var_types = VarTypeMap.empty } exprs in
  ty
