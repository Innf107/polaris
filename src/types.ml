open Syntax
open Syntax.Renamed
open Util

type type_error = UnableToUnify of ty * ty

exception TypeError of loc * type_error

module VarTypeMap = Map.Make(Name)

type ty_constraint = Unify of loc * ty * ty

type global_env = {
  var_types : ty VarTypeMap.t
}

type local_env = {
  local_types : ty VarTypeMap.t;
  constraints : ty_constraint list ref;
}

let unify : local_env -> loc -> ty -> ty -> unit =
  fun env loc ty1 ty2 ->
    env.constraints := (Unify (loc, ty1, ty2) :: !(env.constraints))

let anonymous_unif = Name.{ name = "u"; index = 0 }

let insert_var : name -> ty -> local_env -> local_env =
  fun x ty env -> { env with local_types = VarTypeMap.add x ty env.local_types }

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
      (* TODO: Make sure both sides bind the same set of variables with the smae types *)
      let right_trans = check_pattern env right left_ty in
      left_ty, left_trans << right_trans
      
(* The checking judgement for patterns doesn't actually do anything interesting yet *)
and check_pattern : local_env -> pattern -> ty -> (local_env -> local_env) =
  fun env pat expected_ty ->
    let ty, trans = infer_pattern env pat in
    unify env (get_pattern_loc pat) ty expected_ty;
    trans

let infer_seq : local_env -> expr -> local_env =
  fun env expr -> match expr with
    | LetSeq (loc, pat, e) ->
      let env_trans = todo __LOC__ in
      todo __LOC__
    | _ -> todo __LOC__

let typecheck_top_level : global_env -> expr -> global_env =
  fun global_env expr ->
    let local_env = { local_types = global_env.var_types; constraints = ref [] } in
    let _ = infer_seq local_env expr in
    (* TODO: Solve constraints, apply substitution, generalize, update global_env *)
    global_env

let typecheck exprs = 
  let global_env = { var_types = VarTypeMap.empty } in
  let _ = List.fold_left (fun env e -> typecheck_top_level env e) global_env exprs in
  todo __LOC__
