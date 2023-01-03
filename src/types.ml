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
                | MismatchedTyCon of name * name * ty * ty
                | Impredicative of (ty * ty) * (ty * ty)
                | OccursCheck of Unique.t * name * ty * ty * ty
                | WrongNumberOfArgs of ty list * ty list * ty * ty
                | NonProgCallInPipe of expr
                | MissingRowFields of (string * ty) list * (string * ty) list * ty * ty
                | ArgCountMismatchInDefinition of name * ty list * int
                | NonFunTypeInLetRec of name * ty
                | CannotUnwrapNonData of ty

exception TypeError of loc * type_error

module UniqueMap = Map.Make(Unique)
module UnifSet = Set.Make(struct
  type t = Unique.t * name
  let compare (u1, _) (u2, _) = Unique.compare u1 u2
end)

type ty_constraint = Unify of loc * ty * ty
                   | Unwrap of loc * ty * ty

type global_env = {
  var_types : ty NameMap.t;
  module_var_contents : global_env NameMap.t;
  data_definitions : (name list * ty) NameMap.t;
  type_aliases : (name list * ty) NameMap.t;
}

type local_env = {
  local_types : ty NameMap.t;
  constraints : ty_constraint Difflist.t ref;
  module_var_contents : global_env NameMap.t;
  data_definitions : (name list * ty) NameMap.t;
  type_aliases : (name list * ty) NameMap.t;
}


let replace_unif : Unique.t -> ty -> ty -> ty =
  fun var replacement -> Ty.transform begin function
    | Unif (u, _) when Unique.equal u var -> replacement
    | ty -> ty
    end

module Subst : sig
  type t

  val of_map : ty UniqueMap.t -> t

  val apply : t -> ty -> ty

end = struct
  type t = ty UniqueMap.t 

  let of_map m = m

  let apply subst = Ty.transform begin function
    | Unif (u, _) as ty ->
      begin match UniqueMap.find_opt u subst with
      | None -> ty
      | Some ty' -> ty'
      end
    | ty -> ty
    end
end


let unify : local_env -> loc -> ty -> ty -> unit =
  fun env loc ty1 ty2 ->
    env.constraints := Difflist.snoc !(env.constraints) (Unify (loc, ty1, ty2))

let unwrap_constraint : local_env -> loc -> ty -> ty -> unit =
  fun env loc ty1 ty2 ->
    env.constraints := Difflist.snoc !(env.constraints) (Unwrap (loc, ty1, ty2))

let fresh_unif_raw_with raw_name =
  let index = Unique.fresh() in
  index, Name.{ name = raw_name; index }
    
let fresh_unif_raw () =
  fresh_unif_raw_with "α"

let fresh_unif () = 
  let index, name = fresh_unif_raw () in
  Unif (index, name)

let fresh_unif_with name =
  let index, name = fresh_unif_raw_with name in
  Unif (index, name)

let insert_var : name -> ty -> local_env -> local_env =
  fun x ty env -> { env with local_types = NameMap.add x ty env.local_types }

let insert_data_definition : name -> name list -> ty -> local_env -> local_env =
  fun constructor params underlying_type env ->
    { env with data_definitions = NameMap.add constructor (params, underlying_type) env.data_definitions }

let insert_type_alias : name -> name list -> ty -> local_env -> local_env =
  fun constructor params underlying_type env ->
    { env with type_aliases = NameMap.add constructor (params, underlying_type) env.type_aliases }


let replace_tvar : name -> ty -> ty -> ty =
  fun var replacement -> Ty.transform begin function
    (* We rely on the renamer and generalization to eliminate
       and name shadowing, so we don't need to deal with foralls here *)
    | TyVar tv when tv = var -> replacement
    | ty -> ty
    end

let replace_tvars : ty NameMap.t -> ty -> ty =
  fun vars -> Ty.transform begin function
    | TyVar tv -> begin match NameMap.find_opt tv vars with
      | None -> TyVar tv
      | Some ty -> ty
      end
    | ty -> ty
    end


let rec instantiate_with_function : ty -> (ty * (ty -> ty)) =
  function
  | Forall (tv, ty) ->
    let unif = Unif (Unique.fresh (), tv) in
    (* TODO: Collect all tyvars first to avoid multiple traversals *)
    let replacement_fun = replace_tvar tv unif in
    let instantiated, inner_replacement_fun = instantiate_with_function (replacement_fun ty) in
    instantiated, (fun ty -> inner_replacement_fun (replacement_fun ty))
  | ty -> ty, Fun.id

let instantiate : ty -> ty =
  fun ty ->
    let (instaniated, _) = instantiate_with_function ty in
    instaniated

let rec skolemize_with_function : ty -> ty * (ty -> ty) =
  function
  | Forall (tv, ty) ->
    let skol = Skol (Unique.fresh (), tv) in
    let replacement_fun = replace_tvar tv skol in
    let skolemized, inner_replacement_fun = skolemize_with_function (replacement_fun ty) in
    skolemized, (fun ty -> inner_replacement_fun (replacement_fun ty))
  | ty -> ty, Fun.id

let skolemize : ty -> ty =
  fun ty ->
    let (skolemized, _) = skolemize_with_function ty in
    skolemized
  

let rec eval_module_env : local_env -> module_expr -> global_env =
  fun env -> function
  | ModVar (loc, var) -> 
    begin match NameMap.find_opt var env.module_var_contents with
    | None -> panic __LOC__ (Loc.pretty loc ^ ": Module variable not found in typechecker: '" ^ Name.pretty var ^ "'. This should have been caught earlier!")
    | Some contents -> contents
    end
  | Import ((_, mod_exports, _), path) -> 
    { var_types = mod_exports.exported_variable_types;
      (* TODO: Allow modules to export other modules and include them here *)
      module_var_contents = NameMap.empty;
      data_definitions = mod_exports.exported_data_definitions;
      type_aliases = mod_exports.exported_type_aliases;
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
    | TypePat(_, pattern, ty) ->
      let env_trans = check_pattern env pattern ty in
      ty, env_trans
    | DataPat(loc, constructor_name, pattern) ->
      let type_variables, underlying_type_raw = match NameMap.find_opt constructor_name env.data_definitions with
      | Some (vars, ty) -> vars, ty
      | None -> panic __LOC__ (Loc.pretty loc ^ ": Data constructor not found in typechecker: '" ^ Name.pretty constructor_name ^ "'. This should have been caught earlier!")
      in
      let vars_with_unifs = List.map (fun var -> (var, fresh_unif_with var.name)) type_variables in
      
      let data_type = TyConstructor(constructor_name, List.map snd vars_with_unifs) in

      let underlying_type = replace_tvars (NameMap.of_seq (List.to_seq vars_with_unifs)) underlying_type_raw in

      let env_trans = check_pattern env pattern underlying_type in

      data_type, env_trans      

      
      


(* The checking judgement for patterns doesn't actually do anything interesting at the moment. *)
and check_pattern : local_env -> pattern -> ty -> (local_env -> local_env) =
  fun env pat expected_ty ->
    trace_tc (lazy ("checking pattern '" ^ pretty_pattern pat ^ "' : " ^ pretty_type expected_ty));
    match pat with
    (* We need a special case for var patterns to allow polymorphic type patterns. 
      (These would be rejected by unification)
      This is not necessary for any of the other patterns, since these cannot check against
      polytypes anyway *)
    | VarPat (_, var) -> 
      insert_var var expected_ty
    | _ ->
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
    | DataConstructor(loc, data_name) ->
      begin match NameMap.find_opt data_name env.data_definitions with
      | Some (params, ty) -> 
        let data_constructor_type = 
          List.fold_right (fun param ty -> Forall (param, ty)) params
            (Fun([ty], TyConstructor(data_name, List.map (fun x -> TyVar(x)) params)))
        in
        instantiate data_constructor_type
      | None -> panic __LOC__ ("Unbound data constructor in type checker: '" ^ Name.pretty data_name ^ "'")
    end
    | ModSubscriptDataCon (void, _, _, _) -> absurd void
    | App (loc, func, args) ->
      (* We infer the argument types first and then check the function type against that.
         Since most functions are going to be defined in a let binding, it might be more efficient to
        infer the function type first and either match on that directly or unify with a function type to get the argument types *)
      
      let arg_tys = List.map (infer env) args in
      let result_ty = fresh_unif() in
      trace_tc (lazy ("[Infer App(..)]: Expected fun ty: " ^ pretty_type (Fun (arg_tys, result_ty))));
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
    | UnitLit _ -> Ty.unit
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
    | BinOp (_, expr1, Cons, expr2) ->
      let ty1 = infer env expr1 in
      check env (List ty1) expr2;
      List ty1
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
    | LetSeq _ | LetRecSeq _ | LetEnvSeq _ | LetModuleSeq _ | LetDataSeq _ | LetTypeSeq _ -> panic __LOC__ ("Found LetSeq expression outside of expression block during typechecking")
    | Let (_, p, e1, e2) ->
      let ty, env_trans = infer_pattern env p in
      (* Lets are non-recursive, so we use the *unaltered* environment *)
      check env ty e1;
      infer (env_trans env) e2  
    | LetRec (loc, mty, fun_name, pats, body, rest) ->
      (* We defer to `infer_seq_expr` here, since the core logic is exactly the same *)
      let env_trans = infer_seq_expr env (LetRecSeq(loc, mty, fun_name, pats, body)) in
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
      Ty.unit
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
    | Ascription (_, expr, ty) ->
      check env ty expr;
      ty
    | Unwrap (loc, expr) ->
      let ty = infer env expr in
      let result_ty = fresh_unif () in
      unwrap_constraint env loc ty result_ty;
      result_ty


and check : local_env -> ty -> expr -> unit =
  fun env expected_ty expr ->
    let expected_skolem_ty = skolemize expected_ty in
    let ty = infer env expr in
    unify env (get_loc expr) ty expected_skolem_ty

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
    | LetRecSeq(loc, mty, fun_name, pats, body) ->
      let arg_tys, transformers, result_ty, ty_skolemizer = 
        match Option.map skolemize_with_function mty with
        | None -> 
          let arg_tys, transformres = List.split (List.map (infer_pattern env) pats) in
          arg_tys, transformres, fresh_unif (), Fun.id
        | Some (Fun(arg_tys, result_ty), ty_skolemizer) ->
          if (List.compare_lengths arg_tys pats != 0) then
            raise (TypeError (loc, ArgCountMismatchInDefinition(fun_name, arg_tys, List.length pats)))
          else
            arg_tys, List.map2 (check_pattern env) pats arg_tys, result_ty, ty_skolemizer
        | Some (ty, _) -> raise (TypeError (loc, NonFunTypeInLetRec(fun_name, ty)))
      in


      trace_tc (lazy ("[Infer (LetRec(Seq) ..)]: " ^ Name.pretty fun_name ^ " : " ^ pretty_type (Fun (arg_tys, result_ty))));

      let fun_ty = match mty with
      | Some ty -> ty
      | None -> Fun (arg_tys, result_ty)
      in

      let env_trans = insert_var fun_name fun_ty in

      let inner_env = List.fold_left (<<) Fun.id transformers (env_trans env) in

      let skolemizer_traversal = object(self)
        inherit [unit] Traversal.traversal

        method! ty _ ty = ty_skolemizer ty, ()

        (* ty_skolemizer already traverses the entire type, so this disables
           any duplicate traversals (which would lead to time quadratic in the size of the type) *)
        method! traverse_type state ty = self#ty state ty
      end in

      let body, () = skolemizer_traversal#traverse_expr () body in
      (* Without a type annotation, we have to check the body against a unification variable instead of inferring,
          since we need to know the functions type in its own (possibly recursive) definition*)
      check inner_env result_ty body;
      env_trans
    | LetEnvSeq(loc, envvar, expr) ->
      (* TODO: Once typeclasses are implemented, the expr should really just have to implement
         some kind of 'ToString' typeclass. For now we require the expr to be an exact string though *)
      check env String expr;
      Fun.id
    | LetModuleSeq(loc, name, mod_expr) ->
      let module_env = eval_module_env env mod_expr in
      (* Variables should *not* be merged with the current environment,
         since we still want modules to be qualified (at least by default) 
         
         At the same time, we *do* remove module subscripts from types and data constructors
         so we *need to merge these* with the current environment. 
         (This is fine since data constructor names are always unique, even across modules)
      *)
      fun env -> 
        { env with 
            module_var_contents = NameMap.add name module_env env.module_var_contents;
            data_definitions = NameMap.union (fun _ _ x -> Some x) env.data_definitions module_env.data_definitions;
        }
    | LetDataSeq(loc, data_name, params, ty) ->
      insert_data_definition data_name params ty
    | LetTypeSeq(loc, alias_name, params, ty) ->
      insert_type_alias alias_name params ty
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
      check env Ty.unit expr;
      Fun.id

and infer_seq : local_env -> expr list -> ty =
  fun env exprs -> match exprs with
    | [] -> Ty.unit
    | [ LetSeq _ | LetRecSeq _ | LetEnvSeq _ | LetModuleSeq _ as expr] ->
      (* If the last expression in an expression block is a
         LetSeq* expresion, we don't need to carry the environment transformations,
         but we do have to make sure to check the expression with `infer_seq_expr`, 
         so it is not passed to `infer`
        *)
      let _ : _ = infer_seq_expr env expr in
      Ty.unit
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
  subst : ty UniqueMap.t ref;

  deferred_constraints : ty_constraint Difflist.t ref option
}

let partial_apply state ty =
  Subst.apply (Subst.of_map !(state.subst)) ty

let bind : unify_state -> Unique.t -> name -> ty -> unit =
  fun state u name ty ->
    trace_subst (lazy (pretty_type (Unif (u, name)) ^ " := " ^ pretty_type ty));
    state.subst := UniqueMap.add u ty (UniqueMap.map (replace_unif u ty) !(state.subst)) 

let solve_unify : loc -> local_env -> unify_state -> ty -> ty -> unit =
  fun loc env state original_ty1 original_ty2 ->
    trace_unify (lazy (pretty_type original_ty1 ^ " ~ " ^ pretty_type original_ty2));

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
      | TyConstructor(name1, args1), TyConstructor(name2, args2) ->
        if Name.compare name1 name2 <> 0 then
          raise (TypeError (loc, MismatchedTyCon(name1, name2, partial_apply state original_ty1, partial_apply state original_ty2)))
        else
          if List.compare_lengths args1 args2 <> 0 then
            panic __LOC__ (Loc.pretty loc ^ ": Trying to unify applications of type constructor '" ^ Name.pretty name1 ^ "' to different numbers of arguments.\n    ty1: " ^ pretty_type ty1 ^ "\n    ty2: " ^ pretty_type ty2)
          else
            List.iter2 go args1 args2
      | Fun (dom1, cod1), Fun (dom2, cod2) ->
        if List.compare_lengths dom1 dom2 != 0 then
          raise (TypeError (loc, 
            WrongNumberOfArgs(
              List.map (partial_apply state) dom1,  
              List.map (partial_apply state) dom2, 
              partial_apply state original_ty1, 
              partial_apply state original_ty2)))
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
      | Skol (u1, _), Skol (u2, _) when Unique.equal u1 u2 -> ()
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
        panic __LOC__ (Loc.pretty loc ^ ": Uninstantiated variable row found during unification")
      | TyVar _, _ | _, TyVar _ -> panic __LOC__ (Loc.pretty loc ^ ": Uninstantiated type variable found during unification")
      | TypeAlias (name, args), other_type ->
        let params, underlying_type = match NameMap.find_opt name env.type_aliases with
        | None -> panic __LOC__ (Loc.pretty loc ^ ": Unbound type alias '" ^ Name.pretty name ^ "' in type checker. This should have been caught earlier!")
        | Some (params, underlying_type) -> params, underlying_type
        in
        if List.compare_lengths args params <> 0 then begin
          panic __LOC__ (Loc.pretty loc ^ ": Wrong number of arguments to type alias " ^ Name.pretty name ^ " in type checker. (Expected: " ^ string_of_int (List.length params) ^ ", Actual: " ^ string_of_int (List.length args) ^ " This should have been caught earlier!")
        end;
        let real_type = replace_tvars (NameMap.of_seq (Seq.zip (List.to_seq params) (List.to_seq args))) underlying_type in
        go real_type other_type
      | other_type, TypeAlias (name, args) ->
        let params, underlying_type = match NameMap.find_opt name env.type_aliases with
        | None -> panic __LOC__ (Loc.pretty loc ^ ": Unbound type alias '" ^ Name.pretty name ^ "' in type checker. This should have been caught earlier!")
        | Some (params, underlying_type) -> params, underlying_type
        in
        if List.compare_lengths args params <> 0 then begin
          panic __LOC__ (Loc.pretty loc ^ ": Wrong number of arguments to type alias " ^ Name.pretty name ^ " in type checker. (Expected: " ^ string_of_int (List.length params) ^ ", Actual: " ^ string_of_int (List.length args) ^ " This should have been caught earlier!")
        end;
        let real_type = replace_tvars (NameMap.of_seq (Seq.zip (List.to_seq params) (List.to_seq args))) underlying_type in
        go other_type real_type  
      | _ -> raise (TypeError (loc, UnableToUnify ((partial_apply state ty1, partial_apply state ty2), 
                                                   (partial_apply state original_ty1, partial_apply state original_ty2))))
    in
    go original_ty1 original_ty2

let solve_unwrap : loc -> local_env -> unify_state -> ty -> ty -> unit =
  fun loc env state ty1 ty2 ->
    match partial_apply state ty1 with
    | TyConstructor(name, args) ->
      let var_names, underlying_type_raw = begin match NameMap.find_opt name env.data_definitions with
      | None -> panic __LOC__ (Loc.pretty loc ^ ": Data constructor '" ^ Name.pretty name ^ "' not found in unwrap expression. This should have been caught earlier!")
      | Some (var_names, underlying_type_raw) -> var_names, underlying_type_raw 
      end in
      let underlying_type = replace_tvars (NameMap.of_seq (Seq.zip (List.to_seq var_names) (List.to_seq args))) underlying_type_raw in
      solve_unify loc env state underlying_type ty2
    | ty -> 
      (* Defer this constraint if possible. If not (i.e. we already deferred this one) we throw a type error *)
      match state.deferred_constraints with
      | None -> raise (TypeError(loc, CannotUnwrapNonData ty))
      | Some deferred_constraint_ref ->
        deferred_constraint_ref := Difflist.snoc !deferred_constraint_ref (Unwrap(loc, ty, ty2))

let solve_constraints : local_env -> ty_constraint list -> Subst.t =
  fun env constraints ->
    let go unify_state constraints = 
      List.iter begin function
        | Unify (loc, ty1, ty2) -> 
          solve_unify loc env unify_state ty1 ty2
        | Unwrap (loc, ty1, ty2) ->
          solve_unwrap loc env unify_state ty1 ty2
        end constraints;
    in
    let subst_ref = ref UniqueMap.empty in

    let initial_deferred_constraint_ref = ref Difflist.empty in
    let initial_unify_state = { subst = subst_ref; deferred_constraints = Some initial_deferred_constraint_ref } in
    (* We try to solve the constraints once, collect any deferred ones and try again *)
    go initial_unify_state constraints;
    
    let updated_unify_state = { subst = subst_ref; deferred_constraints = None } in
    go updated_unify_state (Difflist.to_list !initial_deferred_constraint_ref);

    Subst.of_map !subst_ref


let free_unifs : ty -> UnifSet.t =
  Ty.collect (monoid_set (module UnifSet)) begin function
    | Unif (u, name) -> UnifSet.of_list [(u, name)]
    | _ -> UnifSet.empty 
    end

(** Generalizes a given type by turning residual unification variables into
    forall-bound type variables.
    This is the *only* way to introduce forall types right now, 
    since explicit type signatures are not yet supported.  *)
let generalize : ty -> ty =
  fun ty -> 
    let ty' = UnifSet.fold 
      (fun (u, name) r -> 
        let name = Name.refresh name in
        Forall(name, replace_unif u (TyVar name) r)) 
      (free_unifs ty) 
      ty
    in
    trace_subst (lazy ("[generalize] " ^ pretty_type ty ^ " ==> " ^ pretty_type ty'));
    ty'
    
let typecheck_top_level : global_env -> expr -> global_env =
  fun global_env expr ->
    let local_env = 
      { local_types = global_env.var_types;
        module_var_contents = global_env.module_var_contents;
        constraints = ref Difflist.empty;
        data_definitions = global_env.data_definitions;
        type_aliases = global_env.type_aliases;
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
      data_definitions = NameMap.empty;
      type_aliases = NameMap.empty;
    } in

    let subst = solve_constraints local_env (Difflist.to_list !(local_env.constraints)) in

    let global_env = { 
        var_types = 
          NameMap.union (fun _ _ x -> Some x) 
            (global_env.var_types) 
            (NameMap.map (generalize << Subst.apply subst) temp_local_env.local_types);
        module_var_contents =
          NameMap.union (fun _ _ x -> Some x)
            (global_env.module_var_contents)
            temp_local_env.module_var_contents;
        data_definitions =
          NameMap.union (fun _ _ x -> Some x)
            (global_env.data_definitions)
            temp_local_env.data_definitions;
        type_aliases =
          NameMap.union (fun _ _ x -> Some x)
            (global_env.type_aliases)
            temp_local_env.type_aliases
      } in
    global_env

let typecheck_header header env =
  let insert_global_var var ty env =
    { env with var_types = NameMap.add var ty env.var_types }
  in

  let add_var_content env flag_def =

    match flag_def.args with
    | Varargs name ->
      insert_global_var name (List(String)) env
    | Switch name ->
      insert_global_var name Bool env
    | Named names ->
      List.fold_left (fun env name -> insert_global_var name String env) env names
    | NamedDefault names_and_values ->
      List.fold_left (fun env (name, _) -> insert_global_var name String env) env names_and_values
  in
  List.fold_left add_var_content env header.options

let typecheck header exprs = 
  let prim_types = 
    NameMap.of_seq (Seq.map (fun (name, ty) -> ({ name; index=Name.primop_index}, ty)) 
      (Primops.PrimOpMap.to_seq Primops.primops))
  in
  (* TODO: Maybe primops should be part of an implicitly imported module? *)
  let global_env = { 
    var_types = prim_types; 
    module_var_contents = NameMap.empty; 
    data_definitions = NameMap.empty;
    type_aliases = NameMap.empty;
  } in

  let global_env = typecheck_header header global_env in

  List.fold_left (fun env e -> typecheck_top_level env e) global_env exprs 
