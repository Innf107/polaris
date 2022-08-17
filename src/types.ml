open Syntax
open Syntax.Renamed
open Util

type type_error = UnableToUnify of (ty * ty) * (ty * ty)
                                 (* ^           ^ full original types *)
                                 (* | specific mismatch               *)
                | Impredicative of (ty * ty) * (ty * ty)
                | OccursCheck of Unique.t * name * ty * ty * ty

exception TypeError of loc * type_error

module VarTypeMap = Map.Make(Name)
module UniqueMap = Map.Make(Unique)

type ty_constraint = Unify of loc * ty * ty

type global_env = {
  var_types : ty VarTypeMap.t
}

type local_env = {
  local_types : ty VarTypeMap.t;
  constraints : ty_constraint Difflist.t ref;
}

module Subst : sig
  type t

  val empty : t

  val extend : Unique.t -> ty -> t -> t
  
  val merge : t -> t -> t

  val concat : t list -> t

  val apply : t -> ty -> ty
end = struct
  type t = ty UniqueMap.t 

  let empty = UniqueMap.empty

  let extend = UniqueMap.add

  let merge = UniqueMap.union (fun _ _ x -> Some x)

  let concat list = List.fold_right merge list empty

  let rec apply subst = function
    | Unif (u, _) as ty ->
      begin match UniqueMap.find_opt u subst with
      | None -> ty
      | Some ty' -> ty'
      end
    (* Recursive boilerplate *)
    | Forall (a, ty) -> Forall (a, apply subst ty)
    | Fun (dom, cod) -> Fun (List.map (apply subst) dom, apply subst cod)
    | Tuple tys -> Tuple (Array.map (apply subst) tys)
    | List ty -> List (apply subst ty)
    (* unaffected non-recursive cases *)
    | (Number |Var _ | Bool | String) as ty -> ty
end


let unify : local_env -> loc -> ty -> ty -> unit =
  fun env loc ty1 ty2 ->
    env.constraints := Difflist.snoc !(env.constraints) (Unify (loc, ty1, ty2))

let fresh_unif () = Unif (Unique.fresh (), Name.{ name = "u"; index = 0 })

let insert_var : name -> ty -> local_env -> local_env =
  fun x ty env -> { env with local_types = VarTypeMap.add x ty env.local_types }

let rec replace_tvar : name -> ty -> ty -> ty =
  fun var replacement -> 
    let go = replace_tvar var replacement in
    function
    | Forall (tv, ty) ->
      if tv = var then
        (* 'var' has been shadowed. *)
        (* This is probably unnecessary, since the renamer should already rename tvars*)
        Forall (tv, ty)
      else
        Forall (tv, go ty)
    | Var tv ->
      if tv = var then
        replacement
      else
        Var tv
    (* Recursive boilerplate *)
    | Fun (dom, cod) -> Fun (List.map go dom, go cod)
    | Tuple tys ->
      Tuple (Array.map go tys)
    | List ty ->
      List (go ty)
    (* Unaffected non-recursive cases *)
    | (Number | Bool | String | Unif _) as ty -> ty

let rec instantiate : ty -> ty =
  function
  | Forall (tv, ty) ->
    let unif = Unif (Unique.fresh (), tv) in
    (* TODO: Collect all tyvars first to avoid multiple traversals *)
    instantiate (replace_tvar tv unif ty)
  | ty -> ty

let rec infer_pattern : local_env -> pattern -> ty * (local_env -> local_env) =
  fun env pat -> match pat with
    | VarPat (_, x) ->
      let var_ty = fresh_unif () in
      var_ty, insert_var x var_ty
    | ConsPat (_, head_pat, tail_pat) ->
      let elem_ty = fresh_unif () in
      let head_trans = check_pattern env head_pat elem_ty in
      let tail_trans = check_pattern (head_trans env) tail_pat (List elem_ty) in
      (List elem_ty, head_trans << tail_trans)
    | ListPat (_, patterns) ->
      let elem_ty = fresh_unif () in
      let trans = List.fold_left (<<) Fun.id (List.map (fun p -> check_pattern env p elem_ty) patterns) in
      List elem_ty, trans
    | TuplePat (_, patterns) ->
      let pat_tys, transformers = List.split (List.map (infer_pattern env) patterns) in
      let trans = List.fold_left (<<) Fun.id transformers in
      Tuple (Array.of_list pat_tys), trans
    | NumPat (_, _) ->
      Number, Fun.id
    | OrPat (_, left, right) ->
      let left_ty, left_trans = infer_pattern env left in 
      (* TODO: Make sure both sides bind the same set of variables with the same types *)
      let right_trans = check_pattern env right left_ty in
      left_ty, left_trans << right_trans

(* The checking judgement for patterns doesn't actually do anything interesting at the moment. *)
and check_pattern : local_env -> pattern -> ty -> (local_env -> local_env) =
  fun env pat expected_ty ->
    let ty, trans = infer_pattern env pat in
    unify env (get_pattern_loc pat) ty expected_ty;
    trans

let rec infer : local_env -> expr -> ty =
  fun env expr -> match expr with
    | Var (loc, x) -> 
      begin match VarTypeMap.find_opt x env.local_types with
      | Some ty -> instantiate ty
      | None -> panic __LOC__ ("Unbound variable in type checker: '" ^ Name.pretty x ^ "'")
      end
    | App (loc, func, args) ->
      (* We infer the argument types first and then check the function type against that.
         Since most functions are going to be defined in a let binding, it might be more efficient to
        infer the function type first and either match on that directly or unify with a function type to get the argument types *)
      let arg_tys = List.map (infer env) args in
      let result_ty = fresh_unif() in
      check env (Fun (arg_tys, result_ty)) func;
      result_ty
    | Lambda (loc, args, body) -> 
      let arg_tys, transformers = List.split (List.map (fun p -> infer_pattern env p) args) in
      let env = List.fold_left (<<) Fun.id transformers env in 
      let result_ty = infer env body in
      Fun (arg_tys, result_ty)
    | StringLit _ -> String
    | NumLit _ -> Number
    | BoolLit _ -> Bool
    | UnitLit _ -> Tuple [||]
    | NullLit _ -> panic __LOC__ "Nulls should be phased out now that we have static types" 
    | expr -> panic __LOC__ ("NYI: " ^ pretty expr)

and check : local_env -> ty -> expr -> unit =
  fun env expected_ty expr ->
    let ty = infer env expr in
    unify env (get_loc expr) ty expected_ty

and infer_seq_expr : local_env -> expr -> (local_env -> local_env) =
    fun env expr -> match expr with
    | LetSeq (loc, pat, e) ->
      let ty, env_trans = infer_pattern env pat in
      check env ty e;
      env_trans
    | LetRecSeq(loc, fun_name, pats, body) ->
      let arg_tys, transformers = List.split (List.map (infer_pattern env) pats) in
      let result_ty = fresh_unif () in
      (* We have to check the body against a unification variable instead of inferring,
          since we need to know the functions type in its own (possibly recursive) definition*)
      let env_trans = insert_var fun_name (Fun (arg_tys, result_ty)) in
      let inner_env = List.fold_left (<<) Fun.id transformers (env_trans env) in
      check inner_env result_ty body;
      env_trans
    (* TODO: Special case for !p e* expressions nad pipes *)
    (* TODO: Handle remaining LetSeq forms *)
    | expr ->
      check env (Tuple [||]) expr;
      Fun.id

and infer_seq : local_env -> expr list -> ty =
  fun env exprs -> match exprs with
    | [] -> Tuple [||]
    | [ LetSeq _ | LetRecSeq _ | LetEnvSeq _ as expr] ->
      (* If the last expression in an expression block is a
         LetSeq* expresion, we don't need to carry the environment transformations,
         but we do have to make sure to check the expression with `infer_seq_expr`, 
         so it is not passed to `infer_expr`  
        *)
      let _ : _ = infer_seq_expr env expr in
      Tuple [||]
    | [ expr ] -> infer env expr
    | expr :: exprs -> 
      let env_trans = infer_seq_expr env expr in
      infer_seq (env_trans env) exprs

let rec occurs u = function
      | Unif (u2, _) -> Unique.equal u u2
      | Forall (_, ty) -> occurs u ty
      | Fun (dom, cod) -> List.exists (occurs u) dom || occurs u cod
      | List ty -> occurs u ty
      | Tuple tys -> Array.exists (occurs u) tys
      | Number | Bool | String | Var _ -> false

let solve_unify : loc -> ty -> ty -> Subst.t =
  fun loc original_ty1 original_ty2 ->
    let rec go ty1 ty2 = match ty1, ty2 with
      | Unif (u, name), ty | ty, Unif (u, name) -> 
        begin match ty with
        | Unif (u2, _) when u2 = u -> Subst.empty
        | _ -> 
          if occurs u ty then
            raise (TypeError (loc, OccursCheck(u, name, ty, original_ty1, original_ty2)))
          else
            Subst.extend u ty Subst.empty 
        end
      | Fun (dom1, cod1), Fun (dom2, cod2) ->
        Subst.merge (Subst.concat (List.map2 go dom1 dom2)) (go cod1 cod2)
      | Tuple tys1, Tuple tys2 when Array.length tys1 = Array.length tys2 ->
        Array.fold_right Subst.merge (Array.map2 go tys1 tys2) Subst.empty
      | List ty1, List ty2 -> go ty1 ty2
      | (Forall _, _) | (_, Forall _) -> raise (TypeError (loc, Impredicative ((ty1, ty2), (original_ty1, original_ty2))))
      | (Number, Number) | (Bool, Bool) | (String, String) -> Subst.empty
      | Var a, Var b when a = b -> Subst.empty
      | _ -> raise (TypeError (loc, UnableToUnify ((ty1, ty2), (original_ty1, original_ty2))))
    in
    go original_ty1 original_ty2

let solve_constraints : ty_constraint list -> Subst.t =
  let rec go subst = function
    | [] -> subst
    | Unify (loc, ty1, ty2) :: constrs -> 
      let subst = solve_unify loc (Subst.apply subst ty1) (Subst.apply subst ty2) in
      go subst constrs
  in
  go (Subst.empty)

let typecheck_top_level : global_env -> expr -> global_env =
  fun global_env expr ->
    let local_env = { local_types = global_env.var_types; constraints = ref Difflist.empty } in
    
    let local_env_trans = infer_seq_expr local_env expr in
    
    (* This is *extremely hacky* right now.
        We temporarily construct a fake local environment to figure out the top-level local type bindings.
        We then extract those, throw away the collected constraints (they're part of the real local_env anyway)
        and add the bindings to the global environment
    *)
    let temp_local_env = local_env_trans { local_types = VarTypeMap.empty; constraints = ref Difflist.empty} in

    (* TODO: Actually check the expression *)
    let subst = solve_constraints (Difflist.to_list !(local_env.constraints)) in

    let global_env = { 
      global_env with 
        var_types = 
          VarTypeMap.union (fun _ _ x -> Some x) 
            (global_env.var_types) 
            (VarTypeMap.map (Subst.apply subst) temp_local_env.local_types) 
      } in
    (* TODO: generalize *)
    global_env

let typecheck exprs = 
  (* TODO: Include types for primitives, imports and options variables *)
  let global_env = { var_types = VarTypeMap.empty } in
  let _ = List.fold_left (fun env e -> typecheck_top_level env e) global_env exprs in
  todo __LOC__
