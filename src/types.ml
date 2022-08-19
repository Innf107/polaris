open Syntax
open Syntax.Renamed
open Util
open Classes

let tc_category, trace_tc = Trace.make ~flag:"types" ~prefix:"Types" 
let unify_category, trace_unify = Trace.make ~flag:"unify" ~prefix:"Unify"
let subst_category, trace_subst = Trace.make ~flag:"subst" ~prefix: "Subst"

type type_error = UnableToUnify of (ty * ty) * (ty * ty)
                                 (* ^           ^ full original types *)
                                 (* | specific mismatch               *)
                | Impredicative of (ty * ty) * (ty * ty)
                | OccursCheck of Unique.t * name * ty * ty * ty

exception TypeError of loc * type_error

module VarTypeMap = Map.Make(Name)
module UniqueMap = Map.Make(Unique)
module UnifSet = Set.Make(struct
  type t = Unique.t * name
  let compare (u1, _) (u2, _) = Unique.compare u1 u2
end)

type ty_constraint = Unify of loc * ty * ty

type global_env = {
  var_types : ty VarTypeMap.t
}

type local_env = {
  local_types : ty VarTypeMap.t;
  constraints : ty_constraint Difflist.t ref;
}


let replace_unif : Unique.t -> ty -> ty -> ty =
  fun var replacement -> Ty.transform begin function
    | Unif (u, _) when Unique.equal u var -> replacement
    | ty -> ty
    end

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


  let merge = UniqueMap.union (fun _ _ x -> Some x)

  let concat list = List.fold_right merge list empty

  let apply subst = Ty.transform begin function
    | Unif (u, _) as ty ->
      begin match UniqueMap.find_opt u subst with
      | None -> ty
      | Some ty' -> ty'
      end
    | ty -> ty
    end

    let extend = UniqueMap.add  
end


let unify : local_env -> loc -> ty -> ty -> unit =
  fun env loc ty1 ty2 ->
    env.constraints := Difflist.snoc !(env.constraints) (Unify (loc, ty1, ty2))

let fresh_unif () = 
  let index = Unique.fresh() in
  Unif (index, Name.{ name = "u"; index })

let insert_var : name -> ty -> local_env -> local_env =
  fun x ty env -> { env with local_types = VarTypeMap.add x ty env.local_types }

let replace_tvar : name -> ty -> ty -> ty =
  fun var replacement -> Ty.transform begin function
    (* We rely on the renamer and generalization to eliminate
       and name shadowing, so we don't need to deal with foralls here *)
    | Var tv when tv = var -> replacement
    | ty -> ty
    end


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
      trace_tc ("[Infer App(..)]: Expected fun ty: " ^ pretty_type (Fun (arg_tys, result_ty)));
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
    | ListLit (loc, []) ->
      let elem_ty = fresh_unif () in
      List elem_ty
    | ListLit (loc, (expr :: exprs)) ->
      let elem_ty = infer env expr in
      List.iter (check env elem_ty) exprs;
      List elem_ty
    | TupleLit (loc, exprs) ->
      Tuple (Array.map (infer env) (Array.of_list exprs))
    | MapLit _ | MapLookup _ | DynLookup _ -> panic __LOC__ "Typechecking maps is NYI since maps and records are not yet separated and row polymorphism is NYI"
    | Add (_, expr1, expr2) | Sub (_, expr1, expr2) | Mul (_, expr1, expr2) | Div (_, expr1, expr2) ->
      check env Number expr1;
      check env Number expr2;
      Number
    | Concat _ -> panic __LOC__ "Typechecking '~' is NYI, since it would be heavily overloaded and thus require type classes"
    | Equals (_, expr1, expr2) | NotEquals (_, expr1, expr2) ->
      let ty1 = infer env expr1 in
      check env ty1 expr2;
      Bool
    | LE (_, expr1, expr2) | GE (_, expr1, expr2) | LT (_, expr1, expr2) | GT (_, expr1, expr2) ->
      check env Number expr1;
      check env Number expr2;
      Bool
    | Or (_, expr1, expr2) | And (_, expr1, expr2) ->
      check env Bool expr1;
      check env Bool expr2;
      Bool
    | Not (_, expr) ->
      check env Bool expr;
      Bool
    | Range (_, first, last) ->
      check env Number first;
      check env Number last;
      List Number
    | ListComp _ -> todo __LOC__
    | If (_, cond, th, el) ->
      check env Bool cond;
      let res_ty = infer env th in
      (* With subtyping (even subsumption), this might not be correct *)
      check env res_ty el;
      res_ty
    | Seq (_, exprs) -> infer_seq env exprs
    | LetSeq _ | LetRecSeq _ | LetEnvSeq _ -> panic __LOC__ ("Found LetSeq expression outside of expression block during typechecking")
    
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
      trace_tc ("[Infer (LetRecSeq ..)]: " ^ Name.pretty fun_name ^ " : " ^ pretty_type (Fun (arg_tys, result_ty)));
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
         so it is not passed to `infer`
        *)
      let _ : _ = infer_seq_expr env expr in
      Tuple [||]
    | [ expr ] -> infer env expr
    | expr :: exprs -> 
      let env_trans = infer_seq_expr env expr in
      infer_seq (env_trans env) exprs

let rec occurs u = Ty.collect monoid_or begin function
      | Unif (u2, _) -> Unique.equal u u2
      | _ -> false
      end

let solve_unify : loc -> ty -> ty -> Subst.t =
  fun loc original_ty1 original_ty2 ->
    trace_unify (pretty_type original_ty1 ^ " ~ " ^ pretty_type original_ty2);

    let rec go ty1 ty2 = 
      (* TODO: Do this more efficiently *)
      let rec go_list tys1 tys2 = match tys1, tys2 with
        | ty1 :: tys1, ty2 :: tys2 ->
          let subst = go ty1 ty2 in
          Subst.merge subst (go_list (List.map (Subst.apply subst) tys1) (List.map (Subst.apply subst) tys2))
        | [], [] -> Subst.empty
        | tys1, tys2 -> panic __LOC__ ("solve_unify -> go_list: Differently sized type lists")
      in
      match ty1, ty2 with
      | Unif (u, name), ty | ty, Unif (u, name) -> 
        begin match ty with
        (* Ignore 'a ~ a' cpnstraints. These are mostly harmless,
           but might hang the type checker if they become part of the substitution *)
        | Unif (u2, _) when u2 = u -> Subst.empty
        | _ -> 
          if occurs u ty then
            raise (TypeError (loc, OccursCheck(u, name, ty, original_ty1, original_ty2)))
          else
            trace_subst (pretty_type (Unif (u, name)) ^ " := " ^ pretty_type ty);
            Subst.extend u ty Subst.empty 
        end
      | Fun (dom1, cod1), Fun (dom2, cod2) ->
        let subst = go_list dom1 dom2 in
        Subst.merge subst (go (Subst.apply subst cod1) (Subst.apply subst cod2))
      | Tuple tys1, Tuple tys2 when Array.length tys1 = Array.length tys2 ->
        go_list (Array.to_list tys1) (Array.to_list tys2)
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
      let subst = Subst.merge (solve_unify loc (Subst.apply subst ty1) (Subst.apply subst ty2)) subst in
      go subst constrs
  in
  go (Subst.empty)

let free_unifs : ty -> UnifSet.t =
  Ty.collect (monoid_set (module UnifSet)) begin function
    | Unif (u, name) -> UnifSet.of_list [(u, name)]
    | _ -> UnifSet.empty 
    end

(* TODO: What about local let generalization? 
   How the heck does that work in a constraint based typechecker? *)
(** Generalizes a given type by turning residual unification variables into
    forall-bound type variables.
    This is the *only* way to introduce forall tyeps right now, 
    since explicit type signatures are not yet supported.  *)
let generalize : ty -> ty =
  fun ty -> 
    UnifSet.fold 
      (fun (u, name) r -> 
        let name = Name.refresh name in
        Forall(name, replace_unif u (Var name) r)) 
      (free_unifs ty) 
      ty
    
let typecheck_top_level : global_env -> expr -> global_env =
  fun global_env expr ->
    let local_env = { local_types = global_env.var_types; constraints = ref Difflist.empty } in
    
    let local_env_trans = infer_seq_expr local_env expr in
    
    (* This is *extremely hacky* right now.
        We temporarily construct a fake local environment to figure out the top-level local type bindings.
        We then extract those, throw away the collected constraints (they're part of the real local_env anyway)
        and add the bindings to the global environment
    *)
    let temp_local_env = local_env_trans { local_types = VarTypeMap.empty; constraints = local_env.constraints } in

    (* TODO: Actually check the expression *)
    let subst = solve_constraints (Difflist.to_list !(local_env.constraints)) in

    let global_env = { 
      global_env with 
        var_types = 
          VarTypeMap.union (fun _ _ x -> Some x) 
            (global_env.var_types) 
            (VarTypeMap.map (generalize << Subst.apply subst) temp_local_env.local_types) 
      } in
    global_env

let typecheck exprs = 
  (* TODO: Include types for primitives, imports and options variables *)
  let global_env = { var_types = VarTypeMap.empty } in
  let _ = List.fold_left (fun env e -> typecheck_top_level env e) global_env exprs in
  ()
