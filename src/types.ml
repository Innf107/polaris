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

  val apply : t -> ty -> ty
end = struct
  type t = ty UniqueMap.t 

  let empty = UniqueMap.empty

  let extend = UniqueMap.add

  let merge = UniqueMap.union (fun _ _ x -> Some x)

  let rec apply subst = function
    | Unif (u, _) as ty ->
      begin match UniqueMap.find_opt u subst with
      | None -> ty
      | Some ty' -> ty'
      end
    (* Recursive boilerplate *)
    | Forall (a, ty) -> Forall (a, apply subst ty)
    | Tuple tys -> Tuple (Array.map (apply subst) tys)
    | List ty -> List (apply subst ty)
    (* unaffected non-recursive cases *)
    | (Number |Var _ | Bool | String) as ty -> ty
end


let unify : local_env -> loc -> ty -> ty -> unit =
  fun env loc ty1 ty2 ->
    env.constraints := Difflist.snoc !(env.constraints) (Unify (loc, ty1, ty2))

let anonymous_unif = Name.{ name = "u"; index = 0 }

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
      let var_ty = Unif (Unique.fresh (), anonymous_unif) in
      var_ty, insert_var x var_ty
    | ConsPat (_, head_pat, tail_pat) ->
      let elem_ty = Unif (Unique.fresh (), anonymous_unif) in
      let head_trans = check_pattern env head_pat elem_ty in
      let tail_trans = check_pattern (head_trans env) tail_pat (List elem_ty) in
      (List elem_ty, head_trans << tail_trans)
    | ListPat (_, patterns) ->
      let elem_ty = Unif (Unique.fresh (), anonymous_unif) in
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
    | _ -> todo __LOC__

and check : local_env -> ty -> expr -> unit =
  fun env expected_ty expr ->
    let ty = infer env expr in
    unify env (get_loc expr) ty expected_ty

and infer_seq : local_env -> expr list -> ty =
  fun env exprs -> match exprs with
    | [] -> Tuple [||]
    (* TODO: Special case for '!p e*' expressions and pipes *)
    (* TODO: What if 'LetSeq' is the last expression?
       I think it just returns (), so this should be fine? *)
    | [ expr ] -> infer env expr
    | LetSeq (loc, pat, e) :: exprs ->
      let ty, env_trans = infer_pattern env pat in
      check env ty e;
      infer_seq (env_trans env) exprs
    (* TODO: Handle remaining LetSeq forms *)
    | expr :: exprs -> 
      check env (Tuple [||]) expr;
      infer_seq env exprs

let rec occurs u = function
      | Unif (u2, _) -> Unique.equal u u2
      | Forall (_, ty) -> occurs u ty
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
  function
  | [] -> Subst.empty
  | Unify (loc, ty1, ty2) :: constrs -> todo __LOC__

let typecheck_top_level : global_env -> expr -> global_env =
  fun global_env expr ->
    let local_env = { local_types = global_env.var_types; constraints = ref Difflist.empty } in
    let local_env, gloabl_env = match expr with
      | LetSeq (loc, pat, e) ->
        let pat_ty, env_trans = infer_pattern local_env pat in
        (* LetSeq's are still non-recursive *)
        check local_env pat_ty e;
      
        (* This is *extremely hacky* right now.
           We temporarily construct a fake local environment to figure out the top-level local type bindings.
           We then extract those, throw away the collected constraints (they're part of the real local_env anyway)
           and add the bindings to the global environment
        *)
        let temp_local_env = { local_types = VarTypeMap.empty; constraints = ref Difflist.empty} in
      
        let global_env = { global_env with var_types = VarTypeMap.union (fun _ _ x -> Some x) (global_env.var_types) (temp_local_env.local_types) } in
      
        env_trans local_env, global_env
      (* TODO: Handle top-level progcalls and pipes correctly*)
      (* TODO: Handle remaining LetSeq variants*)
      | expr ->
        (* Top level non-definition expressions currently need to have type ().
           This is in line with local expression blocks, but might be a bit inconvenient.
           If we ever decide to allow arbitrary types, we will just have to use 'infer' here instead #
           and ignore the inferred type *)
        check local_env (Tuple [||]) expr;
        local_env, global_env
    in
    (* TODO: Actually check the expression *)
    let subst = solve_constraints (Difflist.to_list !(local_env.constraints)) in
    todo __LOC__
    (* TODO: Solve constraints, apply substitution, generalize, update global_env *)

let typecheck exprs = 
  (* TODO: Include types for primitives, imports and options variables *)
  let global_env = { var_types = VarTypeMap.empty } in
  let _ = List.fold_left (fun env e -> typecheck_top_level env e) global_env exprs in
  todo __LOC__
