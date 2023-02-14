open Syntax
open Syntax.Renamed
open Util

let _tc_category,    trace_tc    = Trace.make ~flag:"types" ~prefix:"Types" 
let _unify_category, trace_unify = Trace.make ~flag:"unify" ~prefix:"Unify"
let _subst_category, trace_subst = Trace.make ~flag:"subst" ~prefix:"Subst"

type type_error = UnableToUnify of (ty * ty) * (ty * ty)
                                 (* ^           ^ full original types *)
                                 (* | specific mismatch               *)
                | DifferentVariantConstrArgs of string * ty list * ty list * ty * ty
                | MismatchedTyCon of name * name * ty * ty
                | Impredicative of (ty * ty) * (ty * ty)
                | OccursCheck of ty Typeref.t * name * ty * ty * ty
                | FunctionsWithDifferentArgCounts of ty list * ty list * ty * ty
                | PassedIncorrectNumberOfArgsToFun of int * ty list * ty
                | IncorrectNumberOfArgsInLambda of int * ty list * ty
                | NonProgCallInPipe of expr
                | MissingRecordFields of (string * ty) list * (string * ty) list * ty * ty
                | MissingVariantConstructors of (string * ty list) list * (string * ty list) list * ty * ty
                | ArgCountMismatchInDefinition of name * ty list * int
                | NonFunTypeInLetRec of name * ty
                | CannotUnwrapNonData of ty

exception TypeError of loc * type_error

module TyperefSet = Set.Make(struct
  type t = ty Typeref.t * name
  let compare (ref1, _) (ref2, _) = Unique.compare (Typeref.get_unique ref1) (Typeref.get_unique ref2)
end)

type ty_constraint = Unify of loc * ty * ty
                   | Unwrap of loc * ty * ty
                   | ProgramArg of loc * ty

type global_env = {
  var_types : Typed.ty NameMap.t;
  module_var_contents : global_env NameMap.t;
  data_definitions : (name list * Typed.ty) NameMap.t;
  type_aliases : (name list * Typed.ty) NameMap.t;
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
    | Unif (typeref, _) when Unique.equal (Typeref.get_unique typeref) var -> replacement
    | ty -> ty
    end

let normalize_unif : ty -> ty =
  function
  | Unif (typeref, name) as ty -> 
    begin match Typeref.get typeref with
    | None -> ty
    | Some inner_ty -> inner_ty
    end
  | RecordUnif (fields, (typeref, name)) as ty ->
    begin match Typeref.get typeref with
    | None -> ty
    | Some inner_ty -> Ty.replace_record_extension fields inner_ty
    end
  | VariantUnif (fields, (typeref, name)) as ty ->
    begin match Typeref.get typeref with
    | None -> ty
    | Some inner_ty -> Ty.replace_variant_extension fields inner_ty
    end  
  | ty -> ty

let unify : local_env -> loc -> ty -> ty -> unit =
  fun env loc ty1 ty2 ->
    match normalize_unif ty1, normalize_unif ty2 with
    (* We can skip obviously equal constraints. 
       This will make debugging vastly simpler and might improve performance a little. *)
    | (Number, Number) | (Bool, Bool)| (String, String) | (RecordClosed [||], RecordClosed [||]) -> ()
    | (TyVar name1, TyVar name2) when Name.equal name1 name2 -> ()
    | (Unif(typeref1, _), Unif(typeref2, _)) when Typeref.equal typeref1 typeref2 -> ()
    | (Skol(u1, _), Skol(u2, _)) when Unique.equal u1 u2 -> ()
    | _ -> env.constraints := Difflist.snoc !(env.constraints) (Unify (loc, ty1, ty2))

(* `subsumes env loc ty1 ty2` asserts that ty1 is meant to be a subtype of ty2.
   
  Right now, subsumption doesn't do anything interesting, since there is no
   subtyping at all yet. This might change in the future, so type checking should
   already use `subsumes` instead of `unify` whenever possible, even if it doesn't make
   a difference yet. *)
let subsumes : local_env -> loc -> ty -> ty -> unit =
  unify

let unwrap_constraint : local_env -> loc -> ty -> ty -> unit =
  fun env loc ty1 ty2 ->
    env.constraints := Difflist.snoc !(env.constraints) (Unwrap (loc, ty1, ty2))

let prog_arg_constraint : local_env -> loc -> ty -> unit =
  fun env loc ty ->
    env.constraints := Difflist.snoc !(env.constraints) (ProgramArg (loc, ty))

let fresh_unif_raw_with raw_name =
  let typeref = Typeref.make () in
  typeref, Name.{ name = raw_name; index = Typeref.get_unique typeref }
    
let fresh_unif_raw () =
  fresh_unif_raw_with "α"

let fresh_unif () = 
  let typeref, name = fresh_unif_raw () in
  Unif (typeref, name)

let fresh_unif_with name =
  let typeref, name = fresh_unif_raw_with name in
  Unif (typeref, name)

let insert_var : name -> ty -> local_env -> local_env =
  fun x ty env -> { env with local_types = NameMap.add x ty env.local_types }

let insert_data_definition : name -> name list -> ty -> local_env -> local_env =
  fun constructor params underlying_type env ->
    { env with data_definitions = NameMap.add constructor (params, underlying_type) env.data_definitions }

let insert_type_alias : name -> name list -> ty -> local_env -> local_env =
  fun constructor params underlying_type env ->
    { env with type_aliases = NameMap.add constructor (params, underlying_type) env.type_aliases }


let replace_tvar : name -> ty -> ty -> ty =
  fun var replacement -> Ty.transform begin fun ty ->
    match normalize_unif ty with
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

let instantiate_type_alias : local_env -> name -> ty list -> ty =
  fun env name args ->
    let params, underlying_type = match NameMap.find_opt name env.type_aliases with
    | None -> panic __LOC__ ("Unbound type alias '" ^ Name.pretty name ^ "' in type checker. This should have been caught earlier!")
    | Some (params, underlying_type) -> params, underlying_type
    in
    if List.compare_lengths args params <> 0 then begin
      panic __LOC__ ("Wrong number of arguments to type alias " ^ Name.pretty name ^ " in type checker. (Expected: " ^ string_of_int (List.length params) ^ ", Actual: " ^ string_of_int (List.length args) ^ " This should have been caught earlier!")
    end;
    replace_tvars (NameMap.of_seq (Seq.zip (List.to_seq params) (List.to_seq args))) underlying_type
    

let rec instantiate_with_function : ty -> (ty * (ty -> ty)) =
  fun ty -> 
  match normalize_unif ty with
  | Forall (tv, ty) ->
    let unif = Unif (Typeref.make (), tv) in
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
  fun ty -> match normalize_unif ty with
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
  

let rec eval_module_env : local_env -> module_expr -> global_env * Typed.module_expr =
  fun env -> function
  | ModVar (loc, var) -> 
    begin match NameMap.find_opt var env.module_var_contents with
    | None -> panic __LOC__ (Loc.pretty loc ^ ": Module variable not found in typechecker: '" ^ Name.pretty var ^ "'. This should have been caught earlier!")
    | Some contents -> contents, ModVar (loc, var)
    end
  | Import ((loc, mod_exports, exprs), path) -> 
    { var_types = mod_exports.exported_variable_types;
      (* TODO: Allow modules to export other modules and include them here *)
      module_var_contents = NameMap.empty;
      data_definitions = mod_exports.exported_data_definitions;
      type_aliases = mod_exports.exported_type_aliases;
    }, Import ((loc, mod_exports, exprs), path)
  | SubModule (loc, mod_expr, name) ->
    let parent_env, mod_expr = eval_module_env env mod_expr in
    match NameMap.find_opt name parent_env.module_var_contents with
    | None -> panic __LOC__ (Loc.pretty loc ^ ": Submodule not found in typechecker: '" ^ Name.pretty name ^ "'. This should have been caught earlier!")
    | Some contents -> contents, SubModule (loc, mod_expr, name)


let rec infer_pattern : local_env -> pattern -> ty * (local_env -> local_env) * Typed.pattern =
  fun env pat -> 
    trace_tc (lazy ("inferring pattern '" ^ pretty_pattern pat ^ "'"));
    match pat with
    | VarPat (loc, varname) ->
      let var_ty = fresh_unif () in
      ( var_ty
      , insert_var varname var_ty
      , VarPat((loc, var_ty), varname)
      )
    | AsPat (loc, pattern, name) ->
      let pattern_ty, env_trans, pattern = infer_pattern env pattern in
      ( pattern_ty
      , insert_var name pattern_ty << env_trans
      , AsPat(loc, pattern, name)
      )
    | ConsPat (loc, head_pattern, tail_pattern) ->
      let elem_ty = fresh_unif () in
      let head_trans, head_pattern = check_pattern env head_pattern elem_ty in
      let tail_trans, tail_pattern = check_pattern (head_trans env) tail_pattern (List elem_ty) in
      ( List elem_ty
      , head_trans << tail_trans
      , ConsPat(loc, head_pattern, tail_pattern)
      )
    | ListPat (loc, patterns) ->
      let elem_ty = fresh_unif () in
      let transformers, patterns = List.split (List.map (fun p -> check_pattern env p elem_ty) patterns) in
      ( List elem_ty
      , Util.compose transformers
      , ListPat (loc, patterns)
      )
    | TuplePat (loc, patterns) ->
      let pattern_types, transformers, patterns = Util.split3 (List.map (infer_pattern env) patterns) in
      ( Tuple (Array.of_list pattern_types)
      , Util.compose transformers
      , TuplePat(loc, patterns)
      )
    | NumPat (loc, number) ->
      ( Number
      , Fun.id
      , NumPat(loc, number)
      )
    | StringPat (loc, literal) ->
      ( String
      , Fun.id
      , StringPat(loc, literal)
      )
    | OrPat (loc, left, right) ->
      let left_ty, left_trans, left = infer_pattern env left in 
      (* TODO: Make sure both sides bind the same set of variables with the same types *)
      let right_trans, right = check_pattern env right left_ty in
      ( left_ty
      , left_trans << right_trans
      , OrPat(loc, left, right)
      )
    | TypePat(loc, pattern, ty) ->
      let env_trans, pattern = check_pattern env pattern ty in
      ( ty
      , env_trans
      , TypePat(loc, pattern, ty)
      )
    | DataPat(loc, constructor_name, pattern) ->
      let type_variables, underlying_type_raw = match NameMap.find_opt constructor_name env.data_definitions with
      | Some (vars, ty) -> vars, ty
      | None -> panic __LOC__ (Loc.pretty loc ^ ": Data constructor not found in typechecker: '" ^ Name.pretty constructor_name ^ "'. This should have been caught earlier!")
      in
      let vars_with_unifs = List.map (fun var -> (var, fresh_unif_with var.name)) type_variables in
      
      let data_type = TyConstructor(constructor_name, List.map snd vars_with_unifs) in

      let underlying_type = replace_tvars (NameMap.of_seq (List.to_seq vars_with_unifs)) underlying_type_raw in

      let env_trans, pattern = check_pattern env pattern underlying_type in

      ( data_type
      , env_trans
      , DataPat(loc, constructor_name , pattern)
      ) 
    | VariantPat(loc, constructor_name, patterns) ->
      (* See Note [Inferring Variant Patterns] *)
      let pattern_types, env_transformers, patterns = Util.split3 (List.map (infer_pattern env) patterns) in

      let extension_unif = fresh_unif_raw () in

      ( VariantUnif([|(constructor_name, pattern_types)|], extension_unif)
      , Util.compose env_transformers
      , VariantPat(loc, constructor_name, patterns)
      )
      
(* Note [Inferring Variant Patterns]
  Inference for variant patterns is quite complicated.

  Naively, one might think that a pattern for constructor `A
  should just be inferred to have the closed variant type type < A() >, since this is exactly what it matches.

  While this would work quite well for let-bound patterns, this breaks down for match expressions.
  Every pattern in a match expression is checked against the type of the scrutinee, meaning every pattern
  needs to have the same type.

  For example, with this approach, the following, obviously sensible declaration would be rejected

  let f(x) = match x {
    A -> 0
    B -> 1
    _ -> 2
  }

  The obvious type for `f` would be `forall r. < A, B | r > -> Number`.
  This leads to the natural conclusion that variant patterns (e.g `A) should always be inferred to 
  an *open* variant type instead (< A | r0 >).

  With this, the example above infers the correct type, but at the cost of breaking correctness!

  let f(x) = match x {
    A -> 0
    B -> 1
  }

  This declaration should be inferred to have type `< A, B > -> Number`, but this approach infers the
  open type `forall r. < A, B | r > -> Number` even thoough the function has no way to handle
  any constructors besides `A and `B!

  Now, how do we infer patterns such that variants are always open when they could match additional 
  constructors at runtime and closed when they could not?

  We cheat.

  Instead of determining this through type inference and unification, we always infer open variant types 
  and later apply separate exhaustiveness analysis to the match expression to close any rows that would be
  non-exhaustive with an open variant type.
  This is the same approach that is taken by OCaml (https://github.com/ocaml/ocaml/blob/trunk/typing/parmatch.ml#L1392)
*)


(* The checking judgement for patterns doesn't actually do anything interesting at the moment. *)
and check_pattern : local_env -> pattern -> ty -> (local_env -> local_env) * Typed.pattern =
  fun env pattern expected_ty ->
    trace_tc (lazy ("checking pattern '" ^ pretty_pattern pattern ^ "' : " ^ pretty_type expected_ty));

    let defer_to_inference () =
      let ty, trans, pattern = infer_pattern env pattern in
      subsumes env (Typed.get_pattern_loc pattern) ty expected_ty;
      trans, pattern
    in
    match pattern, expected_ty with
    (* We need a special case for var patterns to allow polymorphic type patterns. 
      (These would be rejected by unification) *)
    | VarPat (loc, var), expected_ty -> 
      ( insert_var var expected_ty
      , VarPat((loc, expected_ty), var)
      )
    | AsPat (loc, pattern, name), expected_ty ->
      let env_trans, pattern = check_pattern env pattern expected_ty in
      ( insert_var name expected_ty << env_trans
      , AsPat (loc, pattern, name)
      )
    | ConsPat (loc, head, tail), List(elem_ty) ->
      let head_trans, head = check_pattern env head elem_ty in
      let tail_trans, tail = check_pattern env tail (List(elem_ty)) in
      ( tail_trans << head_trans
      , ConsPat(loc, head, tail)
      )
    | ConsPat (loc, _, _), _ -> defer_to_inference ()
    | ListPat (loc, patterns), List(elem_ty) ->
      let transformers, patterns = List.split (List.map (fun pat -> check_pattern env pat elem_ty) patterns) in
      ( Util.compose transformers
      , ListPat(loc, patterns)
      )
    | ListPat _, _ -> defer_to_inference ()
    | TuplePat (loc, patterns), Tuple arg_tys ->
      let transformers, patterns = match Base.List.map2 patterns (Array.to_list arg_tys) ~f:(check_pattern env) with
      | Ok transformers_and_patterns -> List.split transformers_and_patterns
      | Unequal_lengths -> todo __LOC__
      in
      ( Util.compose transformers
      , TuplePat(loc, patterns)
      )
    | TuplePat _, _ -> defer_to_inference ()
    | NumPat(loc, number), Number -> Fun.id, NumPat(loc, number)
    | NumPat _, _ -> defer_to_inference ()
    | StringPat(loc, literal), String -> Fun.id, StringPat(loc, literal)
    | StringPat _, _ -> defer_to_inference ()
    | OrPat(loc, left, right), _ ->
      let left_trans, left = check_pattern env left expected_ty in
      let right_trans, right = check_pattern env right expected_ty in
      ( right_trans << left_trans
      , OrPat(loc, left, right)
      )
    | TypePat(loc, pattern, ty), _ ->
      subsumes env loc ty expected_ty;
      check_pattern env pattern ty
    | DataPat(loc, data_name, underlying_pattern), TyConstructor(constr_name, args) when Name.equal data_name constr_name ->
      begin match NameMap.find_opt data_name env.data_definitions with
      | None -> panic __LOC__ "Unbound data constructor in type checker"
      | Some (type_params, underlying_type_raw) -> 
        let underlying_type = replace_tvars (NameMap.of_seq (Seq.zip (List.to_seq type_params) (List.to_seq args))) underlying_type_raw in
        let env_trans, underlying_pattern = check_pattern env underlying_pattern underlying_type in
        ( env_trans
        , DataPat(loc, data_name, underlying_pattern)
        )
      end
    | DataPat _, _ -> defer_to_inference ()
    | VariantPat _, _ -> 
      defer_to_inference ()

(** Split a function type into its components.
    If the type is not statically known to be a function, this is achieved by unifying
    with a function type consisting of type variables (This is why the number of arguments has to be known beforehand). *)
let rec split_fun_ty : local_env -> loc -> int -> ty -> (ty list * ty) =
  fun env loc arg_count ty -> match normalize_unif ty with
  | Fun(args, result) -> 
    (* TODO: Should we check that the argument count matches here? *)
    (args, result)
  | TypeAlias(alias_name, args) -> 
    let real_type = instantiate_type_alias env alias_name args in
    (* We continue recursively, unwrapping every layer of type synonym until we hit either a function
       or a non-alias type that we can unify with a function type.
       This guarantees that type synonyms for functions taking polytypes behave correctly
       (if rank N types are ever implemented) *)
    split_fun_ty env loc arg_count real_type
  | ty ->
    let argument_types = List.init arg_count (fun _ -> fresh_unif ()) in
    let result_type = fresh_unif () in
    (* TODO: Is the order correct here? Does it even matter? 
       If this is known to be a function (or function alias), we handle it separately
       anyways, so this case should only be triggered (and not fail no matter what we do)
       if the function is a unification variable *)
    subsumes env loc ty (Fun(argument_types, result_type));
    (argument_types, result_type)

(** Find the type under a reference. This will match on the type directly if possible or
    use unification otherwise *)
let rec split_ref_ty : local_env -> loc -> ty -> ty =
  fun env loc ty -> match normalize_unif ty with
  | Ref(ty) -> ty
  | TypeAlias(alias_name, args) ->
    let real_type = instantiate_type_alias env alias_name args in
    split_ref_ty env loc real_type
  | ty -> 
    let inner_type = fresh_unif () in
    subsumes env loc ty (Ref(inner_type));
    inner_type

let rec infer : local_env -> expr -> ty * Typed.expr =
  fun env expr -> 
    trace_tc (lazy ("inferring expression '" ^ pretty expr ^ "'"));
    match expr with
    | Var (loc, x) -> 
      begin match NameMap.find_opt x env.local_types with
      | Some ty -> 
        let instantiated_type = instantiate ty in
        ( instantiated_type
        , Var((loc, instantiated_type), x)
        )
      | None -> panic __LOC__ ("Unbound variable in type checker: '" ^ Name.pretty x ^ "'")
      end
    | DataConstructor(loc, data_name) ->
      begin match NameMap.find_opt data_name env.data_definitions with
      | Some (params, ty) -> 
        let data_constructor_type = 
          List.fold_right (fun param ty -> Forall (param, ty)) params
            (Fun([ty], TyConstructor(data_name, List.map (fun x -> TyVar(x)) params)))
        in
        ( instantiate data_constructor_type
        , DataConstructor(loc, data_name)
        )
      | None -> panic __LOC__ ("Unbound data constructor in type checker: '" ^ Name.pretty data_name ^ "'")
    end
    | VariantConstructor(loc, constructor_name, args) ->
      (* We infer variant constructors to an open variant type.
        The reasoning behind this is that a constructor A(..) can be used as part of
        any set of constructors as long as that set contains A(..), so its type should be
        < A(..) | r0 > *)
      let arg_types, args = List.split (List.map (infer env) args) in
      let ext_field = fresh_unif_raw () in
      ( VariantUnif ([|(constructor_name, arg_types)|], ext_field) 
      , VariantConstructor(loc, constructor_name, args)
      )
    | ModSubscriptDataCon (void, _, _, _) -> absurd void
    | App (loc, fun_expr, args) ->
      (* We infer the function type and then match the arguments against that.
         This is more efficient than inferring the arguments first, since most functions
         are just going to be functions with existing types, so this reduces the amount of
         unification variables and constraints.

         It also makes it possible to add rank N types later, since polytypes can be checked for
         but not inferred. *)
      let fun_ty, fun_expr = infer env fun_expr in
      let (param_tys, result_ty) = split_fun_ty env loc (List.length args) fun_ty in

      let args = match Base.List.map2 param_tys args ~f:(check env) with
      | Ok(args) -> args
      | Unequal_lengths -> raise (TypeError (loc, PassedIncorrectNumberOfArgsToFun(List.length args, param_tys, result_ty)))
      in
      result_ty, App(loc, fun_expr, args)
    | Lambda (loc, args, body) -> 
      let arg_tys, transformers, args = Util.split3 (List.map (fun p -> infer_pattern env p) args) in
      let env = Util.compose transformers env in 
      let result_ty, body = infer env body in
      ( Fun (arg_tys, result_ty)
      , Lambda(loc, args, body) 
      )
    | StringLit (loc, literal) -> String, StringLit(loc, literal)
    | NumLit (loc, literal) -> Number, NumLit (loc, literal)
    | BoolLit (loc, literal) -> Bool, BoolLit (loc, literal)
    | UnitLit loc -> Ty.unit, UnitLit loc
    | ListLit (loc, []) ->
      let elem_ty = fresh_unif () in
      List elem_ty, ListLit(loc, [])
    | ListLit (loc, (expr :: exprs)) ->
      let elem_ty, expr = infer env expr in
      let exprs = List.map (check env elem_ty) exprs in
      ( List elem_ty
      , ListLit (loc, expr :: exprs)
      )
    | TupleLit (loc, exprs) ->
      let elem_types, exprs = List.split (List.map (infer env) exprs) in
      ( Tuple (Array.of_list elem_types)
      , TupleLit (loc, exprs)
      )
    | RecordLit (loc, fields) ->
      let typed_fields = Array.map (fun (x, expr) -> (x, infer env expr)) (Array.of_list fields) in
      ( RecordClosed (Array.map (fun (x, (ty, _)) -> (x, ty)) typed_fields)
      , RecordLit (loc, Array.to_list (Array.map (fun (x, (_, expr)) -> (x, expr)) typed_fields))
      )
    | Subscript (loc, expr, name) -> 
      let val_ty = fresh_unif () in
      let (u, u_name) = fresh_unif_raw () in
      let expr = check env (RecordUnif ([|name, val_ty|], (u, u_name))) expr in
      ( val_ty
      , Subscript (loc, expr, name)
      )
    | ModSubscript (loc, mod_name, key_name) ->
      begin match NameMap.find_opt mod_name env.module_var_contents with
      | None -> panic __LOC__ ("Module not found while typechecking: '" ^ Name.pretty mod_name ^ "'. This should have been caught earlier!")
      | Some mod_env -> begin match NameMap.find_opt key_name mod_env.var_types with
        | None -> panic __LOC__ ("Module does not contain variable: '" ^ Name.pretty key_name ^ "'. This should have been caught earlier!")
        | Some ty -> 
          ( instantiate ty
          , ModSubscript (loc, mod_name, key_name)
          )
        end
      end
    | RecordUpdate (loc, expr, field_updates) ->
      let update_typed_fields = Array.map (fun (x, expr) -> (x, infer env expr)) (Array.of_list field_updates) in
      let unif_raw = fresh_unif_raw () in

      let record_ty = RecordUnif (Array.map (fun (x, (ty, _)) -> (x, ty)) update_typed_fields, unif_raw) in
      let expr = check env record_ty expr in
      ( record_ty
      , RecordUpdate (loc, expr, Array.to_list (Array.map (fun (x, (_, expr)) -> (x, expr)) update_typed_fields))
      )
    | RecordExtension (loc, expr, field_exts) ->
      (* TODO: At the moment, we infer record extensions by checking the underlying field
         against a unification variable and then returning a unification record with the same variable
         as its extension field. It might be more efficient to infer the underlying type properly and
         then merging directly *)
      let field_tys = Array.map (fun (x, expr) -> (x, infer env expr)) (Array.of_list field_exts) in
      let (u, u_name) = fresh_unif_raw () in
      let expr = check env (Unif (u, u_name)) expr in
      ( RecordUnif (Array.map (fun (x, (ty, _)) -> (x, ty)) field_tys, (u, u_name))
      , RecordExtension (loc, expr, Array.to_list (Array.map (fun (x, (_, expr)) -> (x, expr)) field_tys))
      )
    | DynLookup(loc, list_expr, index_expr) ->
      let list_type, list_expr = infer env list_expr in
      let element_type = fresh_unif () in
      subsumes env loc list_type (List element_type);
      let index_expr = check env Number index_expr in
      ( element_type 
      , DynLookup(loc, list_expr, index_expr)
      )

    | BinOp (loc, expr1, ((Add | Sub | Mul | Div) as op), expr2) ->
      let expr1 = check env Number expr1 in
      let expr2 = check env Number expr2 in
      ( Number
      , BinOp (loc, expr1, coerce_bin_op op, expr2)
      )
    | BinOp (loc, expr1, Concat, expr2) ->
      (* TODO: Generalize this to use type classes once they are implemented. 
         Ideally, concat expressions should really just desugar to a type class method *)
      let expr1 = check env String expr1 in
      let expr2 = check env String expr2 in
      ( String
      , BinOp (loc, expr1, Concat, expr2)
      )
    | BinOp (loc, expr1, Cons, expr2) ->
      let ty1, expr1 = infer env expr1 in
      let expr2 = check env (List ty1) expr2 in
      ( List ty1
      , BinOp (loc, expr1, Cons, expr2)
      )
    | BinOp (loc, expr1, ((Equals | NotEquals) as op), expr2) ->
      let ty1, expr1 = infer env expr1 in
      let expr2 = check env ty1 expr2 in
      ( Bool
      , BinOp (loc, expr1, coerce_bin_op op, expr2)
      )
    | BinOp (loc, expr1, ((LE | GE | LT | GT) as op), expr2) ->
      let expr1 = check env Number expr1 in
      let expr2 = check env Number expr2 in
      ( Bool
      , BinOp (loc, expr1, coerce_bin_op op, expr2)
      )
    | BinOp (loc, expr1, ((Or | And) as op), expr2) ->
      let expr1 = check env Bool expr1 in
      let expr2 = check env Bool expr2 in
      ( Bool
      , BinOp (loc, expr1, coerce_bin_op op, expr2)
      )
    | Not (loc, expr) ->
      let expr = check env Bool expr in
      ( Bool
      , Not (loc, expr)
      )
    | Range (loc, first, last) ->
      let first = check env Number first in
      let last = check env Number last in
      ( List Number
      , Range (loc, first, last)
      )
    | ListComp (loc, result, clauses) ->
      let env, clauses = check_list_comp env clauses in
      let ty, result = infer env result in
      ( List ty 
      , ListComp (loc, result, clauses)
      )
    | If (loc, cond, then_branch, else_branch) ->
      let cond = check env Bool cond in
      let res_ty, then_branch = infer env then_branch in
      (* TODO: With subtyping (even subsumption), this might not be correct *)
      let else_branch = check env res_ty else_branch in
      ( res_ty
      , If (loc, cond, then_branch, else_branch)
      )
    | Seq (loc, exprs) -> 
      let ty, exprs = infer_seq env exprs in
      ( ty
      , Seq (loc, exprs)
      )
    | LetSeq _ | LetRecSeq _ | LetEnvSeq _ | LetModuleSeq _ | LetDataSeq _ | LetTypeSeq _ -> panic __LOC__ ("Found LetSeq expression outside of expression block during typechecking")
    | Let (loc, pattern, body, rest) ->
      let ty, env_trans, pattern = infer_pattern env pattern in
      (* Lets are non-recursive, so we use the *unaltered* environment *)
      let body = check env ty body in
      let result_type, rest = infer (env_trans env) rest in
      ( result_type
      , Let (loc, pattern, body, rest)
      )
    | LetRec (loc, mty, fun_name, pats, body, rest) ->
      (* We defer to `check_seq_expr` here, since the core logic is exactly the same *)
      let env_trans, typed_seq_expr = check_seq_expr env (LetRecSeq(loc, mty, fun_name, pats, body)) in
      let (loc, mty, fun_name, pats, body) = match typed_seq_expr with
      | LetRecSeq(loc, mty, fun_name, pats, body) -> loc, mty, fun_name, pats, body
      | _ -> panic __LOC__ (Loc.pretty loc ^ ": check_seq_expr returned non-LetRecSeq expression when passed one")
      in 
      
      let result_type, rest = infer (env_trans env) rest in
      ( result_type
      , LetRec (loc, mty, fun_name, pats, body, rest)
      )
    | LetEnv (loc, envvar, body, rest) ->
      let body = check env String body in
      let result_ty, rest = infer env rest in
      ( result_ty
      , LetEnv(loc, envvar, body, rest) 
      )
    | ProgCall (loc, program, args) ->
      let args_with_types = List.map (infer env) args in
      (* We require a ProgramArgument constraint on every argument for now.
         In the future, this is going to be replaced by a type class,
         which should also be more robust. *)
      List.iter (fun (arg_type, arg) -> prog_arg_constraint env (Typed.get_loc arg) arg_type) args_with_types;

      ( String
      , ProgCall (loc, program, List.map snd args_with_types)
      )
    | Pipe (loc, exprs) -> 
      let rec check_progcalls = function
      | [] -> []
      | (ProgCall _) as expr :: exprs ->
        let _, expr = infer env expr in
        let exprs = check_progcalls exprs in
        (expr :: exprs)
      | (expr :: _) -> raise (TypeError (loc, NonProgCallInPipe expr))
      in
      let expr = match exprs with
      | []-> panic __LOC__ "Empty pipe expression"
      | ((ProgCall _) :: _) -> 
        let exprs = check_progcalls exprs in
        Typed.Pipe(loc, exprs)
      | (expr :: exprs) ->
        (* TODO: Use a toString typeclass *)
        let expr = check env String expr in
        let exprs = check_progcalls exprs in
        Pipe (loc, expr :: exprs)
      in
      String, expr
      
    | EnvVar (loc, name) -> 
      ( String
      , EnvVar (loc, name)
      )
    | Async (loc, expr) ->
      let expr_ty, expr = infer env expr in
      ( Promise expr_ty
      , Async (loc, expr)
      )
    | Await (loc, expr) ->
      let expr_ty = fresh_unif () in
      let expr = check env (Promise expr_ty) expr in
      ( expr_ty
      , Await (loc, expr)
      )
    | Match (loc, scrut, body) ->
      (* TODO: Do exhaustiveness checking. *)
      let scrut_ty, scrut = infer env scrut in
      let result_ty = fresh_unif () in
      let body = body |> List.map begin fun (pat, expr) -> 
        let env_trans, pattern = check_pattern env pat scrut_ty in
        let expr = check (env_trans env) result_ty expr in
        (pattern, expr)
      end in
      ( result_ty
      , Match (loc, scrut, body)
      )
    | Ascription (loc, expr, ty) ->
      let expr = check env ty expr in
      ( ty
      , Ascription (loc, expr, ty)
      )
    | Unwrap (loc, expr) ->
      let ty, expr = infer env expr in
      let result_ty = fresh_unif () in
      unwrap_constraint env loc ty result_ty;
      ( result_ty
      , Unwrap (loc, expr)
      )
    | MakeRef(loc, expr) ->
      let ty, expr = infer env expr in
      ( Ref(ty)
      , MakeRef(loc, expr)
      )
    | Assign (loc, ref_expr, expr) ->
      (* We infer and split the type of the reference and then check the expression against that.
          We could to this in reverse, which would remove the need to split and possibly generate
          fewer constraints, but this might result in drastically worse error messages. *)
      let ref_type, ref_expr = infer env ref_expr in
      let ref_value_type = split_ref_ty env loc ref_type in

      let expr = check env ref_value_type expr in
      ( Ty.unit
      , Assign (loc, ref_expr, expr)
      )  


and check : local_env -> ty -> expr -> Typed.expr =
  fun env expected_ty expr ->
    trace_tc (lazy ("checking expression '" ^ pretty expr ^ "' : " ^ pretty_type expected_ty));
    let expected_ty = skolemize expected_ty in

    let defer_to_inference () =
      let ty, expr = infer env expr in
      subsumes env (Typed.get_loc expr) ty expected_ty;
      expr
    in
    match expr, expected_ty with
    | Var _, _ -> defer_to_inference ()
    | DataConstructor _, _ -> defer_to_inference ()
    (* Variant constructors would be a bit complicated to check directly
       and we wouldn't gain much, so we just defer to inference. *)
    | VariantConstructor _, _ -> defer_to_inference ()
    | ModSubscriptDataCon (void, _, _, _), _ -> absurd void
    | App _, _ -> defer_to_inference ()
    | Lambda(loc, param_patterns, body), expected_ty ->
      let (param_tys, result_ty) = split_fun_ty env loc (List.length param_patterns) expected_ty in
    
      let transformers, param_patterns = match Base.List.map2 param_patterns param_tys ~f:(check_pattern env) with
      | Ok typed_pats -> List.split typed_pats
      | Unequal_lengths -> raise (TypeError (loc, IncorrectNumberOfArgsInLambda(List.length param_patterns, param_tys, result_ty)))
      in
      let env_transformer = Util.compose transformers in
      let body = check (env_transformer env) result_ty body in
      Lambda (loc, param_patterns, body)
    | StringLit (loc, literal), String -> StringLit (loc, literal) 
    | NumLit (loc, literal), Number -> NumLit (loc, literal) 
    | UnitLit loc, RecordClosed [||] -> UnitLit loc
    | ListLit (loc, elems), List(elem_ty) ->
      let elems = List.map (check env elem_ty) elems in
      ListLit (loc, elems)
    | TupleLit (loc, elems), Tuple(elem_tys) ->
      let elems = match Base.List.map2 (Array.to_list elem_tys) elems ~f:(check env) with
      | Ok elems -> elems
      | Unequal_lengths -> todo __LOC__
      in
      TupleLit (loc, elems)
      (* Record literals would be hard to check directly since the fields are order independent,
         so we just defer to inference and let the unifier deal with this. *)
    | RecordLit _, _ -> defer_to_inference ()
    (* Right now, this case is necessary to match type aliases for literal types (e.g. String).
       We could do this manually, but we still need this, since with typeclasses
       there might be actual subtypes for String! (e.g. `forall a. C a => a` where String implements C)
       
       We could check these directly with subsume, but `defer_to_inference` will do this for us and also works for
       cases with parameters *)
    | (StringLit _ | NumLit _ | BoolLit _ | UnitLit _ | ListLit _ | TupleLit _), _ -> defer_to_inference ()
    | Subscript(loc, expr, field), expected_ty -> 
      let ext_field = fresh_unif_raw () in
      let record_ty = RecordUnif ([|field, expected_ty|], ext_field) in
      let expr = check env record_ty expr in
      Subscript (loc, expr, field)
    | ModSubscript _, _ -> defer_to_inference ()
    (* TODO: I'm not sure if we can do this more intelligently / efficiently in check mode *)
    | RecordUpdate _, _ -> defer_to_inference ()
    (* TODO: This can probably be done more efficiently *)
    | RecordExtension _, _ -> defer_to_inference ()
    | DynLookup(loc, list_expr, index_expr), element_type -> 
      let list_expr = check env (List element_type) list_expr in
      let index_expr = check env Number index_expr in
      DynLookup(loc, list_expr, index_expr)
    | BinOp (loc, expr1, Cons, expr2), List(elem_ty) ->
      let expr1 = check env elem_ty expr1 in
      let expr2 = check env (List elem_ty) expr2 in
      BinOp (loc, expr1, Cons, expr2)
    | BinOp _, _ -> defer_to_inference ()
    | Not (loc, expr), Bool ->
      let expr = check env Bool expr in
      Not (loc, expr)
    | Not _, _ -> defer_to_inference ()
    | Range (loc, first, last), List(Number) ->
      let first = check env Number first in
      let last = check env Number last in
      Range (loc, first, last)
    | Range _, _ -> defer_to_inference ()
    | ListComp (loc, result, clauses), (List elem_ty) ->
      let env, clauses = check_list_comp env clauses in
      let result = check env elem_ty result in
      ListComp (loc, result, clauses)
    | ListComp _, _ -> defer_to_inference ()
    | If (loc, cond, then_branch, else_branch), expected_ty ->
      let cond = check env Bool cond in
      let then_branch = check env expected_ty then_branch in
      let else_branch = check env expected_ty else_branch in
      If (loc, cond, then_branch, else_branch)
    | Seq (loc, exprs), expected_ty ->
      let exprs = check_seq env loc expected_ty exprs in
      Seq (loc, exprs)
    | (LetSeq _ | LetRecSeq _ | LetEnvSeq _ | LetModuleSeq _ | LetDataSeq _ | LetTypeSeq _), _ -> panic __LOC__ ("Found LetSeq expression outside of expression block during typechecking")
    | Let (loc, pattern, body, rest), expected_ty ->
      let ty, env_trans, pattern = infer_pattern env pattern in
      (* Lets are non-recursive, so we use the *unaltered* environment *)
      let body = check env ty body in
      let rest = check (env_trans env) expected_ty rest in
      Let (loc, pattern, body, rest)
    | LetRec (loc, mty, fun_name, pats, body, rest), expected_ty ->
      (* We defer to `check_seq_expr` here, since the core logic is exactly the same *)
      let env_trans, expr = check_seq_expr env (LetRecSeq(loc, mty, fun_name, pats, body)) in
      let loc, mty, fun_name, pats, body = match expr with
      | LetRecSeq (loc, mty, fun_name, pats, body) -> loc, mty, fun_name, pats, body
      | _ -> panic __LOC__ (Loc.pretty loc ^ ": check_seq_expr returned non-LetRec expression when passed one")
      in
      
      let rest = check (env_trans env) expected_ty rest in
      LetRec (loc, mty, fun_name, pats, body, rest)
    | LetEnv (loc, envvar, e1, e2), expected_ty ->
      let e1 = check env String e1 in
      let e2 = check env expected_ty e2 in
      LetEnv(loc, envvar, e1, e2)
    | ProgCall _, _ -> defer_to_inference ()
    | Pipe _, _ -> defer_to_inference ()
    | EnvVar _, _ -> defer_to_inference ()
    | Async (loc, expr), Promise(elem_ty) ->
      let expr = check env elem_ty expr in
      Async (loc, expr)
    | Async _, _ -> defer_to_inference ()
    | Await (loc, expr), expected_ty ->
      let expr = check env (Promise expected_ty) expr in
      Await (loc, expr)
    | Match (loc, scrut, body), expected_ty ->
      (* TODO: Do exhaustiveness checking. *)
      let scrut_ty, scrut = infer env scrut in
      let body = body |> List.map begin fun (pat, expr) -> 
        let env_trans, pat = check_pattern env pat scrut_ty in
        pat, check (env_trans env) expected_ty expr  
      end in
      Match(loc, scrut, body)
    | Ascription (loc, expr, ty), expected_ty ->
      let expr = check env ty expr in
      subsumes env loc ty expected_ty;
      Ascription(loc, expr, ty)
    | Unwrap (loc, expr), expected_ty ->
      let ty, expr = infer env expr in
      unwrap_constraint env loc ty expected_ty;
      Unwrap(loc, expr)
    | MakeRef(loc, expr), Ref(inner_type) ->
      check env inner_type expr
    | MakeRef _, _ -> defer_to_inference ()
    | Assign (loc, ref_expr, expr), Ref(inner_type) ->
      let ref_expr = check env (Ref inner_type) ref_expr in
      let expr = check env inner_type expr in
      Assign(loc, ref_expr, expr)
    | Assign _, _ -> defer_to_inference ()
  
and check_list_comp : local_env -> list_comp_clause list -> local_env * Typed.list_comp_clause list =
    fun env -> function
    | [] -> env, []
    | (FilterClause expr :: clauses) ->
      let expr = check env Bool expr in
      let env, clauses = check_list_comp env clauses in
      env, FilterClause expr :: clauses
    | (DrawClause(pattern, expr) :: clauses) ->
      let ty, env_trans, pattern = infer_pattern env pattern in
      (* We use the *unaltered* environment, since
         draw clauses are non-recursive *)
      let expr = check env (List ty) expr in
      let env, clauses = check_list_comp (env_trans env) clauses in
      env, DrawClause(pattern, expr) :: clauses

and check_seq_expr : local_env -> expr -> (local_env -> local_env) * Typed.expr =
    fun env expr -> match expr with
    | LetSeq (loc, pat, e) ->
      let ty, env_trans, pat = infer_pattern env pat in
      let e = check env ty e in
      env_trans, LetSeq(loc, pat, e)
    | LetRecSeq(loc, mty, fun_name, patterns, body) ->
      let arg_tys, transformers, patterns, result_ty, ty_skolemizer = 
        match Option.map skolemize_with_function mty with
        | None -> 
          let arg_tys, transformers, patterns = Util.split3 (List.map (infer_pattern env) patterns) in
          arg_tys, transformers, patterns, fresh_unif (), Fun.id
        | Some (Fun(arg_tys, result_ty), ty_skolemizer) ->            
          let transformers, patterns = match Base.List.map2 patterns arg_tys ~f:(check_pattern env) with
          | Ok transformers_and_patterns -> List.split transformers_and_patterns
          | Unequal_lengths -> raise (TypeError (loc, ArgCountMismatchInDefinition(fun_name, arg_tys, List.length patterns)))
          in
          arg_tys, transformers, patterns, result_ty, ty_skolemizer
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
      let body = check inner_env result_ty body in
      ( env_trans, 
        LetRecSeq (loc, mty, fun_name, patterns, body)
      )
    | LetEnvSeq(loc, envvar, expr) ->
      (* TODO: Once typeclasses are implemented, the expr should really just have to implement
         some kind of 'ToString' typeclass. For now we require the expr to be an exact string though *)
      let expr = check env String expr in
      Fun.id, LetEnvSeq(loc, envvar, expr)
    | LetModuleSeq(loc, name, mod_expr) ->
      let module_env, mod_expr = eval_module_env env mod_expr in
      (* Variables should *not* be merged with the current environment,
         since we still want modules to be qualified (at least by default) 
         
         At the same time, we *do* remove module subscripts from types and data constructors
         so we *need to merge these* with the current environment. 
         (This is fine since data constructor names are always unique, even across modules)
      *)
      (fun env -> 
        { env with 
            module_var_contents = NameMap.add name module_env env.module_var_contents;
            data_definitions = NameMap.union (fun _ _ x -> Some x) env.data_definitions module_env.data_definitions;
        }), LetModuleSeq(loc, name, mod_expr)
    | LetDataSeq(loc, data_name, params, ty) ->
      insert_data_definition data_name params ty, LetDataSeq(loc, data_name, params, ty)
    | LetTypeSeq(loc, alias_name, params, ty) ->
      insert_type_alias alias_name params ty, LetTypeSeq(loc, alias_name, params, ty)
    | ProgCall (loc, prog, args) ->
      let args_with_types = List.map (infer env) args in
      (* See the inference rule for ProgCall expressions *)
      List.iter (fun (arg_type, arg) -> prog_arg_constraint env (Typed.get_loc arg) arg_type) args_with_types;

      Fun.id, ProgCall (loc, prog, List.map snd args_with_types)
    | Pipe _ as expr ->
      (* We defer to `infer` here. We don't care about the result type, since
         a) We know it is `String`
         b) In this context, pipes don't actually return anything, but print to stdout *)
      let _, expr = infer env expr in
      Fun.id, expr
    | expr ->
      let expr = check env Ty.unit expr in
      Fun.id, expr

and infer_seq : local_env -> expr list -> ty * Typed.expr list =
  fun env exprs -> match exprs with
    | [] -> Ty.unit, []
    | [ LetSeq _ | LetRecSeq _ | LetEnvSeq _ | LetModuleSeq _ as expr] ->
      (* If the last expression in an expression block is a
         LetSeq* expresion, we don't need to carry the environment transformations,
         but we do have to make sure to check the expression with `check_seq_expr`, 
         so it is not passed to `infer`
        *)
      let _, expr = check_seq_expr env expr in
      Ty.unit, [ expr ];
    | [ expr ] -> 
      let ty, expr = infer env expr in
      ty, [ expr ]
    | expr :: exprs -> 
      let env_trans, expr = check_seq_expr env expr in
      let env, exprs = infer_seq (env_trans env) exprs in
      env, expr :: exprs
and check_seq : local_env -> loc -> ty -> expr list -> Typed.expr list =
  fun env loc expected_ty exprs -> match exprs with
  | [] -> 
    subsumes env loc expected_ty Ty.unit;
    []
  | [ LetSeq _ | LetRecSeq _ | LetEnvSeq _ | LetModuleSeq _ as expr] ->
    (* If the last expression in an expression block is a
       LetSeq* expresion, we don't need to carry the environment transformations,
       but we do have to make sure to check the expression with `check_seq_expr`, 
       so it is not passed to `check`
      *)
    let _, expr = check_seq_expr env expr in
    [expr]
  | [ expr ] -> 
    [ check env expected_ty expr ]
  | expr :: exprs ->
    let env_trans, expr  = check_seq_expr env expr in
    let exprs = check_seq (env_trans env) loc expected_ty exprs in
    expr :: exprs

let rec occurs needle = Ty.collect Classes.monoid_or begin fun ty ->
    match normalize_unif ty with
    | Unif (typeref, _) when Typeref.equal needle typeref -> true 
    | _ -> false
    end

type unify_state = {
  deferred_constraints : ty_constraint Difflist.t ref option
}

let bind : ty Typeref.t -> name -> ty -> unit =
  fun typeref name ty ->
    trace_subst (lazy (pretty_type (Unif (typeref, name)) ^ " := " ^ pretty_type ty));
    match Typeref.get typeref with
    | Some previous_ty -> panic __LOC__ ("Trying to bind already bound type variable " ^ pretty_name name ^ "$" ^ Unique.display (Typeref.get_unique typeref) ^ ".\n"
                                        ^ "    previous type: " ^ pretty_type previous_ty ^ "\n"
                                        ^ "         new type: " ^ pretty_type ty)
    | None -> Typeref.set typeref ty


let solve_unify : loc -> local_env -> unify_state -> ty -> ty -> unit =
  fun loc env state original_ty1 original_ty2 ->
    trace_unify (lazy (pretty_type original_ty1 ^ " ~ " ^ pretty_type original_ty2));

    let rec go ty1 ty2 = 
      (* `remaining_cont` is called with the remaining fields from both rows,
        that is the fields that are not part of the other row. 
        Closed rows will generally error on this, but unif rows might continue.

        This takes a parameter `unify_fields` that specifies how to unify the row fields.
        For records, this should usually just be `go`, but for variants, where
        individual constructors can take multiple arguments, this should be `go_variant` which is 
        essentially just `List.iter2 go` with parameter count checks
        *)
      (* TODO: Use maps instead of lists to make this more efficient. 
         This might be a bit harder than it sounds, since we have to make sure to
         treat duplicate labels correctly (With the semantics described by Leijen et al) *)
      let unify_rows unify_fields fields1 fields2 remaining_cont =
        let rec go_rows remaining1 = function
          | (field1, ty1)::fields1, fields2 ->
            begin match Util.extract (fun (x, _) -> x = field1) fields2 with
            (* If `field1` is *not* contained in `fields2`, we still have to
               keep it around for `remaining_cont` *)
            | None -> go_rows ((field1, ty1) :: remaining1) (fields1, fields2)
            | Some ((field2, ty2), fields2) ->
              unify_fields field2 ty1 ty2;
              go_rows remaining1 (fields1, fields2)
            end
          | [], remaining2 -> match remaining1, remaining2 with
            | [], [] -> ()
            | _ -> remaining_cont remaining1 remaining2
        in
        go_rows [] (Array.to_list fields1, Array.to_list fields2)
      in
      let go_variant constructor_name params1 params2 =
        if List.compare_lengths params1 params2 <> 0 then
          raise (TypeError (loc, DifferentVariantConstrArgs(constructor_name, params1, params2, original_ty1, original_ty2)))
        else
          List.iter2 go params1 params2
      in
      match normalize_unif ty1, normalize_unif ty2 with
      | Unif (typeref, name), ty | ty, Unif (typeref, name) -> 
        (* Thanks to normalize_unif, we know that these have to be unbound unification variables *)
        begin match normalize_unif ty with
        (* Ignore 'a ~ a' constraints. These are mostly harmless,
          but might hang the type checker if they become part of the substitution 
        *)
        | Unif (typeref2, _) when Typeref.equal typeref typeref2 -> ()
        | ty -> 
          if occurs typeref ty then
            raise (TypeError (loc, OccursCheck(typeref, name, ty, original_ty1, original_ty2)))
          else 
            bind typeref name ty
      end
      | TyConstructor(name1, args1), TyConstructor(name2, args2) ->
        if Name.compare name1 name2 <> 0 then
          raise (TypeError (loc, MismatchedTyCon(name1, name2, original_ty1, original_ty2)))
        else
          if List.compare_lengths args1 args2 <> 0 then
            panic __LOC__ (Loc.pretty loc ^ ": Trying to unify applications of type constructor '" ^ Name.pretty name1 ^ "' to different numbers of arguments.\n    ty1: " ^ pretty_type ty1 ^ "\n    ty2: " ^ pretty_type ty2)
          else
            List.iter2 go args1 args2
      | Fun (dom1, cod1), Fun (dom2, cod2) ->
        if List.compare_lengths dom1 dom2 != 0 then
          raise (TypeError (loc, 
            FunctionsWithDifferentArgCounts(dom1, dom2, original_ty1, original_ty2)))
        else begin
          List.iter2 go dom1 dom2;
          go cod1 cod2
        end
      | Tuple tys1, Tuple tys2 when Array.length tys1 = Array.length tys2 ->
        List.iter2 go (Array.to_list tys1) (Array.to_list tys2)
      | List ty1, List ty2 -> go ty1 ty2
      | Promise ty1, Promise ty2 -> go ty1 ty2
      | Ref ty1, Ref ty2 -> go ty1 ty2
      | (Forall _, _) | (_, Forall _) -> raise (TypeError (loc, Impredicative ((ty1, ty2), (original_ty1, original_ty2))))
      | (Number, Number) | (Bool, Bool) | (String, String) -> ()
      | Skol (u1, _), Skol (u2, _) when Unique.equal u1 u2 -> ()
      (* closed, closed *)
      | RecordClosed fields1, RecordClosed fields2 ->
        unify_rows (fun _ -> go) fields1 fields2 (fun remaining1 remaining2 -> 
          raise (TypeError (loc, MissingRecordFields (remaining1, remaining2, original_ty1, original_ty2))))
      | VariantClosed fields1, VariantClosed fields2 ->
        unify_rows go_variant fields1 fields2 (fun remaining1 remaining2 ->
          raise (TypeError (loc, MissingVariantConstructors (remaining1, remaining2, original_ty1, original_ty2))))
      (* unif, closed *)
      | RecordUnif (fields1, (u, name)), RecordClosed fields2 ->
        unify_rows (fun _ -> go) fields1 fields2 begin fun remaining1 remaining2 ->
          match remaining1 with
          | [] -> bind u name (RecordClosed (Array.of_list remaining2))
          | _ -> raise (TypeError (loc, MissingRecordFields (remaining1, [], original_ty1, original_ty2)))
        end
      | VariantUnif (fields1, (u, name)), VariantClosed fields2 ->
        unify_rows go_variant fields1 fields2 begin fun remaining1 remaining2 ->
          match remaining1 with
          | [] -> bind u name (VariantClosed (Array.of_list remaining2))
          | _ -> raise (TypeError (loc, MissingVariantConstructors (remaining1, [], original_ty1, original_ty2)))
        end
      (* closed, unif *)
      | RecordClosed fields1, RecordUnif (fields2, (u, name)) ->
        unify_rows (fun _ -> go) fields1 fields2 begin fun remaining1 remaining2 ->
          match remaining2 with
          | [] -> bind u name (RecordClosed (Array.of_list remaining1))
          | _ -> raise (TypeError (loc, MissingRecordFields ([], remaining2, original_ty1, original_ty2)))
        end
      | VariantClosed fields1, VariantUnif (fields2, (u, name)) ->
        unify_rows go_variant fields1 fields2 begin fun remaining1 remaining2 ->
          match remaining2 with
          | [] -> bind u name (VariantClosed (Array.of_list remaining1))
          | _ -> raise (TypeError (loc, MissingVariantConstructors ([], remaining2, original_ty1, original_ty2)))
        end
      (* unif, unif *)
      | RecordUnif (fields1, (u1, name1)), RecordUnif (fields2, (u2, name2)) ->
        unify_rows (fun _ -> go) fields1 fields2 begin fun remaining1 remaining2 ->
          let new_u, new_name = fresh_unif_raw_with "µ" in
          bind u1 name1 (RecordUnif (Array.of_list remaining2, (new_u, new_name)));
          bind u2 name2 (RecordUnif (Array.of_list remaining1, (new_u, new_name)))
        end
      | VariantUnif (fields1, (u1, name1)), VariantUnif (fields2, (u2, name2)) ->
        unify_rows go_variant fields1 fields2 begin fun remaining1 remaining2 ->
          let new_u, new_name = fresh_unif_raw_with "µ" in
          bind u1 name1 (VariantUnif (Array.of_list remaining2, (new_u, new_name)));
          bind u2 name2 (VariantUnif (Array.of_list remaining1, (new_u, new_name)))
        end
      (* unif, skolem *)
      (* This is almost exactly like the (unif, closed) case, except that we need to carry the
         skolem extension field over *)
      | RecordUnif (fields1, (unif_unique, unif_name)), RecordSkol (fields2, (skol_unique, skol_name)) ->
        unify_rows (fun _ -> go) fields1 fields2 begin fun remaining1 remaining2 ->
          match remaining1 with
          | [] -> bind unif_unique unif_name (RecordSkol (Array.of_list remaining2, (skol_unique, skol_name)))
          | _ -> raise (TypeError (loc, MissingRecordFields (remaining1, [], original_ty1, original_ty2)))
        end
      | VariantUnif (fields1, (unif_unique, unif_name)), VariantSkol (fields2, (skol_unique, skol_name)) ->
        unify_rows go_variant fields1 fields2 begin fun remaining1 remaining2 ->
          match remaining1 with
          | [] -> bind unif_unique unif_name (VariantSkol (Array.of_list remaining2, (skol_unique, skol_name)))
          | _ -> raise (TypeError (loc, MissingVariantConstructors (remaining1, [], original_ty1, original_ty2)))
        end
      (* skolem, unif *)
      (* This is almost exactly like the (closed, unif) case, except that we need to carry the
         skolem extension field over *)
      | RecordSkol (fields1, (skolem_unique, skolem_name)), RecordUnif (fields2, (unif_unique, unif_name)) -> 
        unify_rows (fun _ -> go) fields1 fields2 begin fun remaining1 remaining2 ->
          match remaining2 with
          | [] -> bind unif_unique unif_name (RecordSkol (Array.of_list remaining1, (skolem_unique, skolem_name)))
          | _ -> raise (TypeError (loc, MissingRecordFields ([], remaining2, original_ty1, original_ty2)))
        end
      | VariantSkol (fields1, (skolem_unique, skolem_name)), VariantUnif (fields2, (unif_unique, unif_name)) -> 
        unify_rows go_variant fields1 fields2 begin fun remaining1 remaining2 ->
          match remaining2 with
          | [] -> bind unif_unique unif_name (VariantSkol (Array.of_list remaining1, (skolem_unique, skolem_name)))
          | _ -> raise (TypeError (loc, MissingVariantConstructors ([], remaining2, original_ty1, original_ty2)))
        end
      (* skolem, skolem *)
      (* Skolem rows only unify if the skolem fields match and
          all fields unify (similar to closed rows) *)
      | RecordSkol (fields1, (skolem1_unique, skolem1_name)), RecordSkol (fields2, (skolem2_unique, skolem2_name)) ->
        (* We unify the skolems to generate a more readable error message.
           TODO: Maybe a custom error message is clearer? *)
        go (Skol (skolem1_unique, skolem1_name)) (Skol (skolem2_unique, skolem2_name));
        unify_rows (fun _ -> go) fields1 fields2 (fun remaining1 remaining2 -> 
          raise (TypeError (loc, MissingRecordFields (remaining1, remaining2, original_ty1, original_ty2))))
      | VariantSkol (fields1, (skolem1_unique, skolem1_name)), VariantSkol (fields2, (skolem2_unique, skolem2_name)) ->
        (* We unify the skolems to generate a more readable error message.
           TODO: Maybe a custom error message is clearer? *)
        go (Skol (skolem1_unique, skolem1_name)) (Skol (skolem2_unique, skolem2_name));
        unify_rows go_variant fields1 fields2 (fun remaining1 remaining2 -> 
          raise (TypeError (loc, MissingVariantConstructors (remaining1, remaining2, original_ty1, original_ty2)))) 
      (* closed, skolem *)
      (* skolem, closed *)
      (* Unifying a skolem and closed record is always impossible so we don't need a dedicated case here. *)
          
      | (RecordVar _, _ | _, RecordVar _ | VariantVar _, _ | _, VariantVar _) -> 
        panic __LOC__ (Loc.pretty loc ^ ": Uninstantiated row variable found during unification")
      | TyVar _, _ | _, TyVar _ -> panic __LOC__ (Loc.pretty loc ^ ": Uninstantiated type variable found during unification")
      | TypeAlias (name, args), other_type ->
        let real_type = instantiate_type_alias env name args in
        go real_type other_type
      | other_type, TypeAlias (name, args) ->
        let real_type = instantiate_type_alias env name args in
        go other_type real_type  
      | _ -> raise (TypeError (loc, UnableToUnify ((ty1, ty2), (original_ty1, original_ty2))))
    in
    go original_ty1 original_ty2

let solve_unwrap : loc -> local_env -> unify_state -> ty -> ty -> unit =
  fun loc env state ty1 ty2 ->
    match normalize_unif ty1 with
    | TyConstructor(name, args) ->
      let var_names, underlying_type_raw = begin match NameMap.find_opt name env.data_definitions with
      | None -> panic __LOC__ (Loc.pretty loc ^ ": Data constructor '" ^ Name.pretty name ^ "' not found in unwrap expression. This should have been caught earlier!")
      | Some (var_names, underlying_type_raw) -> var_names, underlying_type_raw 
      end in
      let underlying_type = replace_tvars (NameMap.of_seq (Seq.zip (List.to_seq var_names) (List.to_seq args))) underlying_type_raw in
      solve_unify loc env state underlying_type ty2
    | Ref(value_type) ->
      solve_unify loc env state value_type ty2
    | ty -> 
      (* Defer this constraint if possible. If not (i.e. we already deferred this one) we throw a type error *)
      match state.deferred_constraints with
      | None -> raise (TypeError(loc, CannotUnwrapNonData ty))
      | Some deferred_constraint_ref ->
        deferred_constraint_ref := Difflist.snoc !deferred_constraint_ref (Unwrap(loc, ty, ty2))

let rec solve_program_arg : loc -> local_env -> unify_state -> ty -> unit =
  fun loc env state ty -> match normalize_unif ty with
  | String | Number -> ()
  | List ty -> solve_program_arg loc env state ty
  | ty -> 
    match state.deferred_constraints with
    | None ->
      (* Fall back to matching against strings. This might be a little
       brittle, but we're going to replace this with type classes in the future anyway. *)
      solve_unify loc env state ty String
    | Some deferred_constraint_ref ->
      deferred_constraint_ref := Difflist.snoc !deferred_constraint_ref (ProgramArg (loc, ty))

let solve_constraints : local_env -> ty_constraint list -> unit =
  fun env constraints ->
    let go unify_state constraints = 
      List.iter begin function
        | Unify (loc, ty1, ty2) -> 
          solve_unify loc env unify_state ty1 ty2
        | Unwrap (loc, ty1, ty2) ->
          solve_unwrap loc env unify_state ty1 ty2
        | ProgramArg (loc, ty) ->
          solve_program_arg loc env unify_state ty
        end constraints;
    in

    let initial_deferred_constraint_ref = ref Difflist.empty in
    let initial_unify_state = { deferred_constraints = Some initial_deferred_constraint_ref } in
    (* We try to solve the constraints once, collect any deferred ones and try again *)
    go initial_unify_state constraints;
    
    let updated_unify_state = { deferred_constraints = None } in
    go updated_unify_state (Difflist.to_list !initial_deferred_constraint_ref)




let free_unifs : ty -> TyperefSet.t =
  Ty.collect (Classes.monoid_set (module TyperefSet)) begin fun ty ->
    match normalize_unif ty with
    | Unif (typeref, name) -> TyperefSet.of_list [(typeref, name)]
    | _ -> TyperefSet.empty 
    end

(** Generalizes a given type by turning residual unification variables into
    forall-bound type variables. 
    This does not currently take the environment into account since only 
    top-level definitions are generalized and these cannot mention free 
    unification variables. 
    This is going to change once we introduce a value restriction!  *)
let generalize : ty -> ty =
  fun ty -> 
    let ty' = TyperefSet.fold 
      (fun (typeref, name) r -> 
        let new_name = Name.refresh name in
        bind typeref name (TyVar new_name);
        Forall(new_name, r)) 
      (free_unifs ty) 
      ty
    in
    trace_tc (lazy ("[generalize] " ^ pretty_type ty ^ " ==> " ^ pretty_type ty'));
    ty'
    
let typecheck_top_level : global_env -> expr -> global_env * Typed.expr =
  fun global_env expr ->
    let local_env = 
      { local_types = global_env.var_types;
        module_var_contents = global_env.module_var_contents;
        constraints = ref Difflist.empty;
        data_definitions = global_env.data_definitions;
        type_aliases = global_env.type_aliases;
      } in
    
    let local_env_trans, expr = check_seq_expr local_env expr in
    
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
            (NameMap.map generalize temp_local_env.local_types);
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
    global_env, expr

let typecheck_exports : Renamed.export_item list -> Typed.export_item list = 
  Obj.magic

let typecheck_header header env =
  let insert_global_var var ty env =
    { env with var_types = NameMap.add var ty env.var_types }
  in

  let add_var_content env flag_def =
    let env, args = match flag_def.args with
    | Varargs name ->
      ( insert_global_var name (List(String)) env
      , Typed.Varargs name
      )
    | Switch name ->
      ( insert_global_var name Bool env
      , Switch name
      )
    | Named names ->
      ( List.fold_left (fun env name -> insert_global_var name String env) env names
      , Named names 
      )
    | NamedDefault names_and_values ->
      ( List.fold_left (fun env (name, _) -> insert_global_var name String env) env names_and_values
      , NamedDefault names_and_values
      )
    in
    env, Typed.{ args; description=flag_def.description; flags=flag_def.flags }
  in
  let env, options = List.fold_left_map add_var_content env header.options in
  env, Typed.{ options; 
          usage = header.usage; 
          description = header.description; 
          exports = typecheck_exports header.exports 
        }

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

  let global_env, header = typecheck_header header global_env in

  let global_env, exprs = List.fold_left_map (fun env e -> typecheck_top_level env e) global_env exprs in
  (global_env, header, exprs)

