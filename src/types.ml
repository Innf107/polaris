open Syntax
open Syntax.Renamed
open Util
open Classes

let _tc_category, trace_tc = Trace.make ~flag:"types" ~prefix:"Types" 
let _unify_category, trace_unify = Trace.make ~flag:"unify" ~prefix:"Unify"
let _subst_category, trace_subst = Trace.make ~flag:"subst" ~prefix: "Subst"

type type_error = UnableToUnify of (ty * ty) * (ty * ty)
                                 (* ^           ^ full original types *)
                                 (* | specific mismatch               *)
                | Impredicative of (ty * ty) * (ty * ty)
                | OccursCheck of Unique.t * name * ty * ty * ty
                | WrongNumberOfArgs of ty list * ty list * ty * ty
                | NonProgCallInPipe of expr
                | MissingRowFields of (string * ty) list * (string * ty) list * ty * ty

exception TypeError of loc * type_error

module UniqueMap = Map.Make(Unique)
module UnifSet = Set.Make(struct
  type t = Unique.t * name
  let compare (u1, _) (u2, _) = Unique.compare u1 u2
end)

type ty_constraint = Unify of loc * ty * ty

type global_env = {
  var_types : ty NameMap.t;
  module_var_contents : global_env NameMap.t;
}

type local_env = {
  local_types : ty NameMap.t;
  constraints : ty_constraint Difflist.t ref;
  module_var_contents : global_env NameMap.t
}


let replace_unif : Unique.t -> ty -> ty -> ty =
  fun var replacement -> Ty.transform begin function
    | Unif (u, _) when Unique.equal u var -> replacement
    | ty -> ty
    end

module Subst : sig
  type t

  val of_map : ty UniqueMap.t -> t

  val empty : t

  val extend : Unique.t -> ty -> t -> t

  val merge : t -> t -> t

  val concat : t list -> t

  val apply : t -> ty -> ty

end = struct
  type t = ty UniqueMap.t 

  let empty = UniqueMap.empty

  let of_map m = m

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

let fresh_unif_raw_with raw_name =
  let index = Unique.fresh() in
  index, Name.{ name = raw_name; index }
    
let fresh_unif_raw () =
  fresh_unif_raw_with "α"

let fresh_unif () = 
  let index, name = fresh_unif_raw () in
  Unif (index, name)

let insert_var : name -> ty -> local_env -> local_env =
  fun x ty env -> { env with local_types = NameMap.add x ty env.local_types }

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

let rec eval_module_env : local_env -> module_expr -> global_env =
  fun env -> function
  | ModVar (loc, var) -> 
    begin match NameMap.find_opt var env.module_var_contents with
    | None -> panic __LOC__ (Loc.pretty loc ^ ": Module variable not found in typechecker: '" ^ Name.pretty var ^ "'. This should have been caught earlier!")
    | Some contents -> contents
    end
  | Import ((_, mod_exports, _), path) -> 
    { var_types = mod_exports.exported_types;
      (* TODO: Allow modules to export other modules and include them here *)
      module_var_contents = NameMap.empty;
    }
  | SubModule (loc, mod_expr, name) ->
    let parent_env = eval_module_env env mod_expr in
    match NameMap.find_opt name parent_env.module_var_contents with
    | None -> panic __LOC__ (Loc.pretty loc ^ ": Submodule not found in typechecker: '" ^ Name.pretty name ^ "'. This should have been caught earlier!")
    | Some contents -> contents


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
      begin match NameMap.find_opt x env.local_types with
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
    | TupleLit (_, exprs) ->
      Tuple (Array.map (infer env) (Array.of_list exprs))
    | RecordLit (_, fields) ->
      let ty_fields = Array.map (fun (x, expr) -> (x, infer env expr)) (Array.of_list fields) in
      Record (RowClosed ty_fields)
    | Subscript (_, expr, name) -> 
      let val_ty = fresh_unif () in
      let (u, u_name) = fresh_unif_raw () in
      check env (Record (RowUnif ([|name, val_ty|], (u, u_name)))) expr;
      val_ty
    | ModSubscript (_, mod_name, key_name) ->
      begin match NameMap.find_opt mod_name env.module_var_contents with
      | None -> panic __LOC__ ("Module not found while typechecking: '" ^ Name.pretty mod_name ^ "'. This should have been caught earlier!")
      | Some mod_env -> begin match NameMap.find_opt key_name mod_env.var_types with
        | None -> panic __LOC__ ("Module does not contain variable: '" ^ Name.pretty key_name ^ "'. This should have been caught earlier!")
        | Some ty -> instantiate ty
        end
      end
    | RecordUpdate (_, expr, field_updates) ->
      let update_tys = Array.map (fun (x, expr) -> (x, infer env expr)) (Array.of_list field_updates) in
      let unif_raw = fresh_unif_raw () in
      let record_ty = (Record (RowUnif (update_tys, unif_raw))) in
      check env record_ty expr;
      record_ty      
    | RecordExtension (_, expr, field_exts) ->
      let field_tys = Array.map (fun (x, expr) -> (x, infer env expr)) (Array.of_list field_exts) in
      let (u, u_name) = fresh_unif_raw () in
      check env (Unif (u, u_name)) expr;
      Record (RowUnif (field_tys, (u, u_name)))
    | DynLookup _ -> todo __LOC__
    | BinOp (_, expr1, (Add | Sub | Mul | Div), expr2) ->
      check env Number expr1;
      check env Number expr2;
      Number
    | BinOp (_, expr1, Concat, expr2) ->
      (* TODO: Generalize this to use type classes once they are implemented. 
         Ideally, concat expressions should really just desugar to a type class method *)
      check env String expr1;
      check env String expr2;
      String
    | BinOp (_, expr1, (Equals | NotEquals), expr2) ->
      let ty1 = infer env expr1 in
      check env ty1 expr2;
      Bool
    | BinOp (_, expr1, (LE | GE | LT | GT), expr2) ->
      check env Number expr1;
      check env Number expr2;
      Bool
    | BinOp (_, expr1, (Or | And), expr2) ->
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
    | ListComp (_, result, clauses) ->
      let env = infer_list_comp env clauses in
      let ty = infer env result in
      List ty
    | If (_, cond, th, el) ->
      check env Bool cond;
      let res_ty = infer env th in
      (* With subtyping (even subsumption), this might not be correct *)
      check env res_ty el;
      res_ty
    | Seq (_, exprs) -> infer_seq env exprs
    | LetSeq _ | LetRecSeq _ | LetEnvSeq _ | LetModuleSeq _ -> panic __LOC__ ("Found LetSeq expression outside of expression block during typechecking")
    | Let (_, p, e1, e2) ->
      let ty, env_trans = infer_pattern env p in
      (* Lets are non-recursive, so we use the *unaltered* environment *)
      check env ty e1;
      infer (env_trans env) e2  
    | LetRec (loc, fun_name, pats, body, rest) ->
      (* We defer to `infer_seq_expr` here, since the core logic is exactly the same *)
      let env_trans = infer_seq_expr env (LetRecSeq(loc, fun_name, pats, body)) in
      infer (env_trans env) rest
    | LetEnv (_, envvar, e1, e2) ->
      check env String e1;
      infer env e2
    | Assign (loc, var, expr) ->
      let var_ty = match NameMap.find_opt var env.local_types with
        | Some ty -> ty
        | None -> panic __LOC__ (Loc.pretty loc ^ ": Unbound variable in typechecker (assignment): '" ^ Name.pretty var ^ "'")
      in
      check env var_ty expr;
      Tuple [||]
    | ProgCall (loc, prog, args) ->
      (* TODO: We really need some kind of toString typeclass here *)
      List.iter (check env String) args;
      String
    | Pipe (loc, exprs) -> 
      let rec check_progcalls = function
      | [] -> ()
      | (ProgCall _) as expr :: exprs ->
        let _ = infer env expr in
        check_progcalls exprs
      | (expr :: _) -> raise (TypeError (loc, NonProgCallInPipe expr))
      in
      begin match exprs with
      | []-> panic __LOC__ "Empty pipe expression"
      | ((ProgCall _) :: _) -> check_progcalls exprs
      | (expr :: exprs) ->
        (* TODO: Use a toString typeclass *)
        check env String expr;
        check_progcalls exprs
      end;
      String
    | EnvVar _ -> String
    | Async (_, expr) ->
      let expr_ty = infer env expr in
      Promise expr_ty
    | Await (_, expr) ->
      let expr_ty = fresh_unif () in
      check env (Promise expr_ty) expr;
      expr_ty
    | Match (_, scrut, body) ->
      (* TODO: Do exhaustiveness checking. This should probably be done in the renamer *)
      let scrut_ty = infer env scrut in
      let result_ty = fresh_unif () in
      body |> List.iter begin fun (pat, expr) -> 
        let env_trans = check_pattern env pat scrut_ty in
        check (env_trans env) result_ty expr  
      end;
      result_ty 

and check : local_env -> ty -> expr -> unit =
  fun env expected_ty expr ->
    let ty = infer env expr in
    unify env (get_loc expr) ty expected_ty

and infer_list_comp : local_env -> list_comp_clause list -> local_env =
    fun env -> function
    | [] -> env
    | (FilterClause expr :: clauses) ->
      check env Bool expr;
      infer_list_comp env clauses
    | (DrawClause(pat, expr) :: clauses) ->
      let ty, env_trans = infer_pattern env pat in
      (* We use the *unaltered* environment, since
         draw clauses are non-recursive *)
      check env (List ty) expr;
      infer_list_comp (env_trans env) clauses

and infer_seq_expr : local_env -> expr -> (local_env -> local_env) =
    fun env expr -> match expr with
    | LetSeq (loc, pat, e) ->
      let ty, env_trans = infer_pattern env pat in
      check env ty e;
      env_trans
    | LetRecSeq(loc, fun_name, pats, body) ->
      let arg_tys, transformers = List.split (List.map (infer_pattern env) pats) in
      let result_ty = fresh_unif () in
      trace_tc ("[Infer (LetRec(Seq) ..)]: " ^ Name.pretty fun_name ^ " : " ^ pretty_type (Fun (arg_tys, result_ty)));
      (* We have to check the body against a unification variable instead of inferring,
          since we need to know the functions type in its own (possibly recursive) definition*)
      let env_trans = insert_var fun_name (Fun (arg_tys, result_ty)) in
      let inner_env = List.fold_left (<<) Fun.id transformers (env_trans env) in
      check inner_env result_ty body;
      env_trans
    | LetEnvSeq(loc, envvar, expr) ->
      (* TODO: Once typeclasses are implemented, the expr should really just have to implement
         some kind of 'ToString' typeclass. For now we require the expr to be an exact string though *)
      check env String expr;
      Fun.id
    | LetModuleSeq(loc, name, mod_expr) ->
      let module_env = eval_module_env env mod_expr in
      (* TODO: This should *not* be merged with the current environment,
         since we still want modules to be qualified (at least by default) *)
      fun env -> { env with module_var_contents = NameMap.add name module_env env.module_var_contents }
    | ProgCall (loc, prog, args) ->
      List.iter (check env String) args;
      Fun.id
    | Pipe _ as expr ->
      (* We defer to `infer` here. We don't care about the result type, since
         a) We know it is `String`
         b) In this context, pipes don't actually return anything, but print to stdout *)
      let _ = infer env expr in
      Fun.id
    | expr ->
      check env (Tuple [||]) expr;
      Fun.id

and infer_seq : local_env -> expr list -> ty =
  fun env exprs -> match exprs with
    | [] -> Tuple [||]
    | [ LetSeq _ | LetRecSeq _ | LetEnvSeq _ | LetModuleSeq _ as expr] ->
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

let rec occurs subst u = Ty.collect monoid_or begin function
      | Unif (u2, _) -> 
        if Unique.equal u u2 then
          true
        else
          begin match UniqueMap.find_opt u2 !subst with
          | None -> false
          | Some ty -> occurs subst u ty
          end
      | _ -> false
      end

type unify_state = {
  (* This uses a 'ref' instead of a mutable field to allow the
     possibility of adding immutable fields later *)
  subst : ty UniqueMap.t ref
}

let partial_apply state ty =
  Subst.apply (Subst.of_map !(state.subst)) ty

let bind : unify_state -> Unique.t -> name -> ty -> unit =
  fun state u name ty ->
    trace_subst (pretty_type (Unif (u, name)) ^ " := " ^ pretty_type ty);
    state.subst := UniqueMap.add u ty (UniqueMap.map (replace_unif u ty) !(state.subst)) 

let solve_unify : loc -> unify_state -> ty -> ty -> unit =
  fun loc state original_ty1 original_ty2 ->
    trace_unify (pretty_type original_ty1 ^ " ~ " ^ pretty_type original_ty2);

    let rec go ty1 ty2 = 
      (* `remaining_cont` is called with the remaining fields from both rows,
        that is the fields that are not part of the other row. 
        Closed rows will generally error on this, but unif rows might continue 
        *)
      (* TODO: Use maps instead of lists to make this more efficient. 
         This might be a bit harder than it sounds, since we have to make sure to
         treat duplicate labels correctly (With the semantics described by Leijen et al) *)
      let unify_rows fields1 fields2 remaining_cont =
        let rec go_rows remaining1 = function
          | (field1, ty1)::fields1, fields2 ->
            begin match Util.extract (fun (x, _) -> x = field1) fields2 with
            (* If `field1` is *not* contained in `fields2`, we still have to
               keep it around for `remaining_cont` *)
            | None -> go_rows ((field1, ty1) :: remaining1) (fields1, fields2)
            | Some ((field2, ty2), fields2) ->
              go ty1 ty2;
              go_rows remaining1 (fields1, fields2)
            end
          | [], remaining2 -> match remaining1, remaining2 with
            | [], [] -> ()
            | _ -> remaining_cont remaining1 remaining2
        in
        go_rows [] (Array.to_list fields1, Array.to_list fields2)
      in
      match ty1, ty2 with
      | Unif (u, name), ty | ty, Unif (u, name) -> 
        begin match UniqueMap.find_opt u !(state.subst) with
        | Some subst_ty -> go subst_ty ty
        | None ->
          begin match ty with
          (* Ignore 'a ~ a' constraints. These are mostly harmless,
            but might hang the type checker if they become part of the substitution 
          *)
          | Unif (u2, _) when u = u2 -> ()
          | Unif (u2, _) -> 
            begin match UniqueMap.find_opt u2 !(state.subst) with
            | Some subst_ty -> go (Unif (u, name)) subst_ty
            | None -> bind state u name ty
            end
          | _ -> 
            if occurs state.subst u ty then
              raise (TypeError (loc, OccursCheck(u, name, partial_apply state ty, partial_apply state original_ty1, partial_apply state original_ty2)))
            else begin
              bind state u name ty
          end
        end
      end
      | Fun (dom1, cod1), Fun (dom2, cod2) ->
        if List.compare_lengths dom1 dom2 != 0 then
          raise (TypeError (loc, WrongNumberOfArgs(dom1, dom2, original_ty1, original_ty2)))
        else begin
          List.iter2 go dom1 dom2;
          go cod1 cod2
        end
      | Tuple tys1, Tuple tys2 when Array.length tys1 = Array.length tys2 ->
        List.iter2 go (Array.to_list tys1) (Array.to_list tys2)
      | List ty1, List ty2 -> go ty1 ty2
      | Promise ty1, Promise ty2 -> go ty1 ty2
      | (Forall _, _) | (_, Forall _) -> raise (TypeError (loc, Impredicative ((ty1, ty2), (original_ty1, original_ty2))))
      | (Number, Number) | (Bool, Bool) | (String, String) -> ()
      | Record (RowClosed fields1), Record (RowClosed fields2) ->
        unify_rows fields1 fields2 (fun remaining1 remaining2 -> 
          raise (TypeError (loc, MissingRowFields (remaining1, remaining2, original_ty1, original_ty2))))
      | Record (RowUnif (fields1, (u, name))), Record (RowClosed fields2) ->
        begin match UniqueMap.find_opt u !(state.subst) with
        | Some subst_ty -> go (replace_unif u subst_ty ty1) ty2
        | None -> 
          unify_rows fields1 fields2 begin fun remaining1 remaining2 ->
            match remaining1 with
            | [] -> bind state u name (Record (RowClosed (Array.of_list remaining2)))
            | _ -> raise (TypeError (loc, MissingRowFields (remaining1, [], original_ty1, original_ty2)))
          end
        end
      | Record (RowClosed fields1), Record (RowUnif (fields2, (u, name))) ->
        begin match UniqueMap.find_opt u !(state.subst) with
        | Some subst_ty -> go ty1 (replace_unif u subst_ty ty2) 
        | None -> 
          unify_rows fields1 fields2 begin fun remaining1 remaining2 ->
            match remaining2 with
            | [] -> bind state u name (Record (RowClosed (Array.of_list remaining1)))
            | _ -> raise (TypeError (loc, MissingRowFields ([], remaining2, original_ty1, original_ty2)))
          end
        end
      | Record (RowUnif (fields1, (u1, name1))), Record (RowUnif (fields2, (u2, name2))) ->
        begin match UniqueMap.find_opt u1 !(state.subst) with
        | Some subst_ty -> go (replace_unif u1 subst_ty ty1) ty2
        | None -> match UniqueMap.find_opt u2 !(state.subst) with
          | Some subst_ty -> go ty1 (replace_unif u2 subst_ty ty2)
          | None ->
            unify_rows fields1 fields2 begin fun remaining1 remaining2 ->
              let new_u, new_name = fresh_unif_raw_with "µ" in
              bind state u1 name1 (Record (RowUnif (Array.of_list remaining2, (new_u, new_name))));
              bind state u2 name2 (Record (RowUnif (Array.of_list remaining1, (new_u, new_name))))
          end
        end
      | (Record (RowVar _), _ | _, Record (RowVar _)) -> 
        panic __LOC__ "Uninstantiated variable row found during unification"
      | Var _, _ | _, Var _ -> panic __LOC__ "Uninstantiated type variable found during unification"
      | _ -> raise (TypeError (loc, UnableToUnify ((ty1, ty2), (original_ty1, original_ty2))))
    in
    go original_ty1 original_ty2

let solve_constraints : ty_constraint list -> Subst.t =
  fun constraints ->
  let unify_state = { subst = ref UniqueMap.empty } in
  List.iter begin function
    | Unify (loc, ty1, ty2) -> 
      solve_unify loc unify_state ty1 ty2
    end constraints;
  Subst.of_map !(unify_state.subst)  

let free_unifs : ty -> UnifSet.t =
  Ty.collect (monoid_set (module UnifSet)) begin function
    | Unif (u, name) -> UnifSet.of_list [(u, name)]
    | _ -> UnifSet.empty 
    end

(* TODO: What about local let generalization? 
   How the heck does that work in a constraint based typechecker? *)
(** Generalizes a given type by turning residual unification variables into
    forall-bound type variables.
    This is the *only* way to introduce forall types right now, 
    since explicit type signatures are not yet supported.  *)
let generalize : ty -> ty =
  fun ty -> 
    let ty' = UnifSet.fold 
      (fun (u, name) r -> 
        let name = Name.refresh name in
        Forall(name, replace_unif u (Var name) r)) 
      (free_unifs ty) 
      ty
    in
    trace_subst ("[generalize] " ^ pretty_type ty ^ " ==> " ^ pretty_type ty');
    ty'
    
let typecheck_top_level : global_env -> expr -> global_env =
  fun global_env expr ->
    let local_env = 
      { local_types = global_env.var_types;
        module_var_contents = global_env.module_var_contents;
        constraints = ref Difflist.empty;
      } in
    
    let local_env_trans = infer_seq_expr local_env expr in
    
    (* This is *extremely hacky* right now.
        We temporarily construct a fake local environment to figure out the top-level local type bindings.
        We then extract those, throw away the collected constraints (they're part of the real local_env anyway)
        and add the bindings to the global environment
    *)
    let temp_local_env = local_env_trans { 
      local_types = NameMap.empty;
      module_var_contents = NameMap.empty;
      constraints = local_env.constraints; 
    } in

    let subst = solve_constraints (Difflist.to_list !(local_env.constraints)) in

    let global_env = { 
      global_env with 
        var_types = 
          NameMap.union (fun _ _ x -> Some x) 
            (global_env.var_types) 
            (NameMap.map (generalize << Subst.apply subst) temp_local_env.local_types);
        module_var_contents =
          NameMap.union (fun _ _ x -> Some x)
            (global_env.module_var_contents)
            temp_local_env.module_var_contents
      } in
    global_env

let typecheck exprs = 
  let prim_types = 
    NameMap.of_seq (Seq.map (fun (name, ty) -> ({ name; index=Name.primop_index}, ty)) 
      (Primops.PrimOpMap.to_seq Primops.primops))
  in
  (* TODO: Include types for imports and options variables *)
  (* TODO: Maybe primops should be part of an implicitly imported module? *)
  let global_env = { var_types = prim_types; module_var_contents = NameMap.empty } in
  List.fold_left (fun env e -> typecheck_top_level env e) global_env exprs 
