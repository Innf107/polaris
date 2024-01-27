open Syntax
open Util
module StringSet = Set.Make (String)
module RenameMap = Map.Make (String)
module FilePathMap = Map.Make (String)

type rename_error =
  | VarNotFound of string * loc
  | ModuleVarNotFound of string * loc
  | TyVarNotFound of string * loc
  | TyConNotFound of string * loc
  | DataConNotFound of string * loc
  | TooManyArgsToDataConPattern of name * Renamed.pattern list * loc
  | SubscriptVarNotFound of string * loc
  | LetSeqInNonSeq of Parsed.expr * loc
  | SubModuleNotFound of string * loc
  | WrongNumberOfTyConArgs of name * int * Parsed.ty list * loc
  | NonExceptionInTry of name * loc
  | UnboundExportConstructor of string * loc
  | DuplicateKeyInRecordUpdate of string * loc
  | NonClassInConstraint of Renamed.ty * loc
  | NonClassInInstance of name * loc
  | ClassMethodMismatch of {
      class_name : name;
      missing : string list;
      invalid : string list;
      loc : loc;
    }
  | WrongNumberOfClassArgs of {
      class_name : name;
      expected : int;
      actual : int;
      loc : loc;
    }

exception RenameError of rename_error

module RenameScope = struct
  open RenameMap

  type t = {
    variables : name RenameMap.t;
    module_vars : (name * t) RenameMap.t;
    ty_vars : name RenameMap.t;
    (* Polaris does not have a Haskell-style kind system, so we
       just check that type constructors are always fully applied in the renamer. *)
    ty_constructors : (name * int * type_constructor_sort) RenameMap.t;
    type_aliases : (name list * Renamed.ty) NameMap.t;
    data_constructors : (name * data_constructor_sort) RenameMap.t;
    type_classes : (name list * (name * Renamed.ty) StringMap.t) NameMap.t;
  }

  let empty : t =
    {
      variables =
        RenameMap.mapi
          (fun name _ -> { name; index = Name.primop_index })
          Primops.primops;
      module_vars = RenameMap.empty;
      ty_vars = RenameMap.empty;
      ty_constructors = RenameMap.empty;
      type_aliases = NameMap.empty;
      data_constructors =
        RenameMap.map
          (fun (name, _) -> (name, ExceptionSort))
          Primops.prim_exceptions;
      type_classes = NameMap.empty;
    }

  let insert_var (old : string) (renamed : name) (scope : t) : t =
    { scope with variables = add old renamed scope.variables }

  let insert_mod_var (old : string) (renamed : name) (contents : t) (scope : t)
      : t =
    { scope with module_vars = add old (renamed, contents) scope.module_vars }

  let insert_type_var (old : string) (renamed : name) (scope : t) : t =
    { scope with ty_vars = add old renamed scope.ty_vars }

  let insert_type_constructor old renamed arg_count sort scope =
    {
      scope with
      ty_constructors = add old (renamed, arg_count, sort) scope.ty_constructors;
    }

  let insert_type_alias name parameters underlying scope =
    {
      scope with
      type_aliases =
        NameMap.add name (parameters, underlying) scope.type_aliases;
    }

  let insert_data_constructor old renamed sort scope =
    {
      scope with
      data_constructors = add old (renamed, sort) scope.data_constructors;
    }

  let insert_type_class class_name params method_names scope =
    {
      scope with
      type_classes =
        NameMap.add class_name (params, method_names) scope.type_classes;
    }

  let lookup_var (scope : t) (loc : loc) (var : string) : name =
    try find var scope.variables with
    | Not_found -> raise (RenameError (VarNotFound (var, loc)))

  let lookup_data (scope : t) (loc : loc) (data : string) :
      name * data_constructor_sort =
    try find data scope.data_constructors with
    | Not_found -> raise (RenameError (DataConNotFound (data, loc)))

  let lookup_mod_var (scope : t) (loc : loc) (var : string) : name * t =
    try find var scope.module_vars with
    | Not_found -> raise (RenameError (ModuleVarNotFound (var, loc)))
end

let fresh_var = Name.fresh

let instantiate_type_alias scope name args =
  let params, underlying_type =
    match NameMap.find_opt name RenameScope.(scope.type_aliases) with
    | None ->
        panic __LOC__
          ("Unbound type alias '" ^ Name.pretty name
         ^ "' in renamer instantiation")
    | Some (params, underlying_type) -> (params, underlying_type)
  in
  if List.compare_lengths args params <> 0 then begin
    panic __LOC__
      ("Wrong number of arguments to type alias " ^ Name.pretty name
     ^ " in renamer instantiation. (Expected: "
      ^ string_of_int (List.length params)
      ^ ", Actual: "
      ^ string_of_int (List.length args)
      ^ " This should have been caught earlier!")
  end;
  let variable_substitutions =
    NameMap.of_seq (Seq.zip (List.to_seq params) (List.to_seq args))
  in

  let substitute_variable = function
    | Renamed.TyVar name -> begin
        match NameMap.find_opt name variable_substitutions with
        | Some substitution -> substitution
        | None ->
            panic __LOC__
              ("Unbound variable in type alias during substitution: "
             ^ Name.pretty name)
      end
    | ty -> ty
  in

  Renamed.Traversal.transform_type substitute_variable underlying_type

let rename_type_constructor :
    (Parsed.ty -> Renamed.ty) ->
    loc ->
    RenameScope.t ->
    string ->
    Parsed.ty list ->
    Renamed.ty =
 fun rename_nonbinding loc scope name args ->
  let name, arg_count, sort =
    match RenameMap.find_opt name scope.ty_constructors with
    | None -> raise (RenameError (TyConNotFound (name, loc)))
    | Some (name, arg_count, sort) -> (name, arg_count, sort)
  in
  if List.compare_length_with args arg_count <> 0 then begin
    raise (RenameError (WrongNumberOfTyConArgs (name, arg_count, args, loc)))
  end;
  let arguments = List.map rename_nonbinding args in
  begin
    match sort with
    | TypeAliasSort ->
        let underlying = instantiate_type_alias scope name arguments in
        TypeAlias { name; arguments; underlying }
    | DataConSort -> TyConstructor (name, arguments)
    | ClassSort -> TyConstructor (name, arguments)
  end

let rename_type loc (scope : RenameScope.t) original_type =
  let rec go (scope : RenameScope.t) ty =
    let rec rename_nonbinding (scope : RenameScope.t) = function
      | Parsed.Number -> Renamed.Number
      | Bool -> Bool
      | String -> String
      | Exception -> Exception
      | List ty -> List (rename_nonbinding scope ty)
      | Promise ty -> Promise (rename_nonbinding scope ty)
      | Ref ty -> Ref (rename_nonbinding scope ty)
      | Tuple tys -> Tuple (Array.map (rename_nonbinding scope) tys)
      | Fun (tys1, ty) ->
          Fun
            ( List.map (rename_nonbinding scope) tys1,
              (rename_nonbinding scope) ty )
      | Constraint (class_constraint, ty) ->
          let class_constraint = rename_nonbinding scope class_constraint in

          let class_name, args =
            match class_constraint with
            | TyConstructor (class_name, args) -> (class_name, args)
            | _ ->
                raise
                  (RenameError (NonClassInConstraint (class_constraint, loc)))
          in

          begin
            match NameMap.find_opt class_name scope.type_classes with
            | None ->
                raise
                  (RenameError (NonClassInConstraint (class_constraint, loc)))
            | Some _ -> Constraint (class_constraint, rename_nonbinding scope ty)
          end
      | RecordClosed tys ->
          RecordClosed
            (Array.map (fun (x, ty) -> (x, rename_nonbinding scope ty)) tys)
      | VariantClosed tys ->
          VariantClosed
            (Array.map
               (fun (x, ty) -> (x, List.map (rename_nonbinding scope) ty))
               tys)
      | RecordVar (tys, varname) -> begin
          match RenameMap.find_opt varname scope.ty_vars with
          | Some varname ->
              RecordVar
                ( Array.map (fun (x, ty) -> (x, rename_nonbinding scope ty)) tys,
                  varname )
          | None -> raise (RenameError (TyVarNotFound (varname, loc)))
        end
      | VariantVar (tys, varname) -> begin
          match RenameMap.find_opt varname scope.ty_vars with
          | Some varname ->
              VariantVar
                ( Array.map
                    (fun (x, ty) -> (x, List.map (rename_nonbinding scope) ty))
                    tys,
                  varname )
          | None -> raise (RenameError (TyVarNotFound (varname, loc)))
        end
      | TyConstructor (name, args) ->
          rename_type_constructor (rename_nonbinding scope) loc scope name args
      | ModSubscriptTyCon ((), mod_name, name, args) ->
          let _, module_export_scope =
            RenameScope.lookup_mod_var scope loc mod_name
          in
          (* The renamed name is guaranteed to be unique among all modules so we just get rid of the
              ModSubscriptTyCon for later passes
              TODO: The module might still be useful for debugging, so we should probably include it in the name somehow
          *)
          rename_type_constructor (rename_nonbinding scope) loc
            module_export_scope name args
      | TypeAlias _ ->
          panic __LOC__ (Loc.pretty loc ^ ": Type Alias found before renamer")
      | TyVar tv -> begin
          match RenameMap.find_opt tv scope.ty_vars with
          | Some tv' -> TyVar tv'
          | None -> raise (RenameError (TyVarNotFound (tv, loc)))
        end
      | Parsed.Forall (tyvar, ty) ->
          let tyvar' = fresh_var tyvar in
          let ty =
            rename_nonbinding
              (RenameScope.insert_type_var tyvar tyvar' scope)
              ty
          in
          Forall (tyvar', ty)
      | Unif _ ->
          panic __LOC__
            "Unification variable found after parsing. How did this happen wtf?"
      | Skol _ ->
          panic __LOC__ "Skolem found after parsing. How did this happen wtf?"
      | RecordUnif _
      | VariantUnif _ ->
          panic __LOC__
            "Unification variable row found after parsing. How did this happen \
             wtf?"
      | RecordSkol _
      | VariantSkol _ ->
          panic __LOC__
            "Skolem row found after parsing. How did this happen wtf?"
    in
    match ty with
    | Parsed.Forall (tv, ty) ->
        let tv' = fresh_var tv in
        let scope_trans = RenameScope.insert_type_var tv tv' in
        let ty', other_trans = go (scope_trans scope) ty in
        (Renamed.Forall (tv', ty'), fun scope -> scope_trans (other_trans scope))
    | _ -> (rename_nonbinding scope ty, Fun.id)
  in
  go scope original_type

(* Note [PatternTypeTransformers]
   rename_pattern returns *two* scope transformers. One for value bindings, as one might expect,
   and one for type variable bindings.
   The utility of this second transformer is a bit less obvious. Unlike the value transformer,
   which specifies how to bind the matched values for the *remaining code*, this one is only
   relevant for the *definition* of whatever is bound by the pattern (currently only used in let bindings).

   This is necessary so that let bound variables with universally quantified types behave correctly, e.g.

    ```
    # This should be accepted and the type of x should refer to the type variable bound by f's type
    let f : forall a. a = \(x : a) -> x

    # This should be rejected
    let y : a = ...
    ```
*)
let rec rename_pattern (or_bound_variables : name RenameMap.t)
    (scope : RenameScope.t) =
  let open RenameScope in
  function
  | Parsed.VarPat (loc, var) ->
      (* If a variable has been bound in a previous branch of an or-pattern, we resolve it to that one
         so that variables in different branches match up correctly *)
      let var' =
        match RenameMap.find_opt var or_bound_variables with
        | Some bound_var -> bound_var
        | None -> fresh_var var
      in
      (Renamed.VarPat (loc, var'), insert_var var var', Fun.id)
  | AsPat (loc, pattern, string_name) ->
      let pattern, env_trans, ty_trans =
        rename_pattern or_bound_variables scope pattern
      in
      let name = fresh_var string_name in
      ( AsPat (loc, pattern, name),
        insert_var string_name name << env_trans,
        ty_trans )
  | ConsPat (loc, x, xs) ->
      let x', x_trans, x_ty_trans = rename_pattern or_bound_variables scope x in
      let xs', xs_trans, xs_ty_trans =
        rename_pattern or_bound_variables scope xs
      in
      ( ConsPat (loc, x', xs'),
        (fun scope -> xs_trans (x_trans scope)),
        fun scope -> xs_ty_trans (x_ty_trans scope) )
  | ListPat (loc, pats) ->
      let pats', pats_trans, pats_ty_trans =
        Util.split3 (List.map (rename_pattern or_bound_variables scope) pats)
      in
      (ListPat (loc, pats'), Util.compose pats_trans, Util.compose pats_ty_trans)
  | TuplePat (loc, pats) ->
      let pats', pats_trans, pats_ty_trans =
        Util.split3 (List.map (rename_pattern or_bound_variables scope) pats)
      in
      ( TuplePat (loc, pats'),
        Util.compose pats_trans,
        Util.compose pats_ty_trans )
  | NumPat (loc, f) -> (NumPat (loc, f), (fun x -> x), fun x -> x)
  | StringPat (loc, literal) -> (StringPat (loc, literal), Fun.id, Fun.id)
  | OrPat (loc, p1, p2) ->
      let p1', p1_trans, p1_ty_trans =
        rename_pattern or_bound_variables scope p1
      in

      (* Hacky way to add the variables bound in p1 to the ones bound in the surrounding scope.
         We need this since any variable bound in the first branch should be resolved to exactly the same name
         in the second branch. *)
      let or_bound_variables =
        (p1_trans RenameScope.{ empty with variables = or_bound_variables })
          .variables
      in

      let p2', p2_trans, p2_ty_trans =
        rename_pattern or_bound_variables scope p2
      in

      ( OrPat (loc, p1', p2'),
        (fun scope -> p2_trans (p1_trans scope)),
        fun scope -> p2_ty_trans (p1_ty_trans scope) )
  | TypePat (loc, p, ty) ->
      let p', p_trans, p_ty_trans = rename_pattern or_bound_variables scope p in
      let ty', ty_trans = rename_type loc scope ty in
      (TypePat (loc, p', ty'), p_trans, fun scope -> ty_trans (p_ty_trans scope))
  | DataPat (loc, constructor_name, pattern) ->
      let pattern, scope_transformer, type_transformer =
        rename_pattern or_bound_variables scope pattern
      in
      begin
        match RenameMap.find_opt constructor_name scope.data_constructors with
        | Some (constructor_name, NewtypeConSort) ->
            ( DataPat (loc, constructor_name, pattern),
              scope_transformer,
              type_transformer )
        | Some (constructor_name, ExceptionSort) ->
            ( ExceptionDataPat (loc, constructor_name, [ pattern ]),
              scope_transformer,
              type_transformer )
        | None ->
            ( VariantPat (loc, constructor_name, [ pattern ]),
              scope_transformer,
              type_transformer )
      end
  | VariantPat (loc, constructor_name, patterns) ->
      let patterns, scope_transformers, type_transformers =
        Util.split3
          (List.map (rename_pattern or_bound_variables scope) patterns)
      in
      begin
        match RenameMap.find_opt constructor_name scope.data_constructors with
        | Some (constructor_name, NewtypeConSort) ->
            raise
              (RenameError
                 (TooManyArgsToDataConPattern (constructor_name, patterns, loc)))
        | Some (constructor_name, ExceptionSort) ->
            ( ExceptionDataPat (loc, constructor_name, patterns),
              Util.compose scope_transformers,
              Util.compose type_transformers )
        | None ->
            ( VariantPat (loc, constructor_name, patterns),
              Util.compose scope_transformers,
              Util.compose type_transformers )
      end
  | ExceptionDataPat (loc, name, _) ->
      panic __LOC__
        (Loc.pretty loc ^ ": Exception data pattern for exception '" ^ name
       ^ "' before renaming")

let rename_pattern = rename_pattern RenameMap.empty

let rename_patterns scope pats =
  List.fold_right
    (fun pat (pats', trans, ty_trans) ->
      begin
        let pat', pat_trans, pat_ty_trans = rename_pattern scope pat in
        ( pat' :: pats',
          (fun scope -> pat_trans (trans scope)),
          fun scope -> pat_ty_trans (ty_trans scope) )
      end)
    pats
    ([], (fun x -> x), fun x -> x)

let rec rename_mod_expr :
    (module_exports * Typed.expr list) FilePathMap.t ->
    RenameScope.t ->
    Parsed.module_expr ->
    Renamed.module_expr * RenameScope.t =
 fun exports scope -> function
  | ModVar (loc, mod_var) ->
      let name, contents = RenameScope.lookup_mod_var scope loc mod_var in
      (ModVar (loc, name), contents)
  | Import (loc, path) -> begin
      match FilePathMap.find_opt path exports with
      | None ->
          panic __LOC__
            ("import path not found in renamer: '" ^ path ^ "'.\nImports: ["
            ^ String.concat ", " (List.map fst (FilePathMap.bindings exports))
            ^ "]")
      | Some (mod_exports, body) ->
          let scope =
            RenameScope.empty
            |> StringMap.fold
                 (fun name renamed r -> RenameScope.insert_var name renamed r)
                 mod_exports.exported_variables
            |> StringMap.fold
                 (fun name (renamed, arg_count, sort) r ->
                   RenameScope.insert_type_constructor name renamed arg_count
                     sort
                     (match sort with
                     | DataConSort ->
                         RenameScope.insert_data_constructor name renamed
                           NewtypeConSort r
                     | TypeAliasSort -> r
                     | ClassSort -> r))
                 mod_exports.exported_ty_constructors
            |> NameMap.fold
                 (fun name _ r ->
                   RenameScope.insert_data_constructor name.name name
                     ExceptionSort r)
                 mod_exports.exported_exceptions
            |> NameMap.fold
                 (fun name (params, methods) r ->
                   RenameScope.insert_type_constructor name.name name
                     (List.length params) ClassSort
                     (RenameScope.insert_type_class name params
                        (StringMap.of_seq
                           (Seq.map
                              (fun (name, ty) -> (name.name, (name, ty)))
                              (List.to_seq methods)))
                        r))
                 mod_exports.exported_type_classes
          in
          (Import ((loc, mod_exports, body), path), scope)
    end
  | SubModule (loc, mod_expr, field) ->
      let mod_expr', contents = rename_mod_expr exports scope mod_expr in
      let field', sub_contents =
        match StringMap.find_opt field contents.module_vars with
        | None -> raise (RenameError (SubModuleNotFound (field, loc)))
        | Some (field', sub_contents) -> (field', sub_contents)
      in
      (SubModule (loc, mod_expr', field'), sub_contents)

let rename_binop : Parsed.binop -> Renamed.binop = function
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div -> Div
  | Concat -> Concat
  | Cons -> Cons
  | Equals -> Equals
  | NotEquals -> NotEquals
  | LE -> LE
  | GE -> GE
  | LT -> LT
  | GT -> GT
  | Or -> Or
  | And -> And

let rec rename_expr (exports : (module_exports * Typed.expr list) FilePathMap.t)
    (scope : RenameScope.t) (expr : Parsed.expr) : Renamed.expr =
  let open RenameScope in
  match expr with
  | Var (loc, var_name) -> Var (loc, lookup_var scope loc var_name)
  | VariantConstructor (loc, name, args) ->
      let args = List.map (rename_expr exports scope) args in
      VariantConstructor (loc, name, args)
  | DataConstructor (loc, constructor_name) -> begin
      match RenameMap.find_opt constructor_name scope.data_constructors with
      | Some (constructor_name, NewtypeConSort) ->
          DataConstructor (loc, constructor_name)
      | Some (constructor_name, ExceptionSort) ->
          ExceptionConstructor (loc, constructor_name)
      | None -> VariantConstructor (loc, constructor_name, [])
    end
  | ExceptionConstructor (loc, _) ->
      panic __LOC__
        (Loc.pretty loc ^ ": Invalid exception constructor before renamer")
  (* We need this special case since data constructors are represented as unapplied values
     (similar to variables), whereas variant constructors always have to appear fully applied.
     (Otherwise their type would be ambiguous if we want to allow `A to be equivalent to `A() ) *)
  | App (loc, DataConstructor (constructor_loc, constructor_name), args) ->
      let args = List.map (rename_expr exports scope) args in
      begin
        match RenameMap.find_opt constructor_name scope.data_constructors with
        | Some (constructor_name, NewtypeConSort) ->
            App (loc, DataConstructor (constructor_loc, constructor_name), args)
        | Some (constructor_name, ExceptionSort) ->
            App
              ( loc,
                ExceptionConstructor (constructor_loc, constructor_name),
                args )
        | None -> VariantConstructor (loc, constructor_name, args)
      end
  | ModSubscriptDataCon ((), loc, mod_name, name) ->
      let _, module_export_scope =
        RenameScope.lookup_mod_var scope loc mod_name
      in

      let name, sort = RenameScope.lookup_data module_export_scope loc name in
      (* We get rid of the module subscript expression part, since the data constructor name
         is guaranteed to be unique among all modules so we don't need it after the renamer anymore *)
      begin
        match sort with
        | NewtypeConSort -> DataConstructor (loc, name)
        | ExceptionSort -> ExceptionConstructor (loc, name)
      end
  | App (loc, f, args) ->
      App
        ( loc,
          rename_expr exports scope f,
          List.map (rename_expr exports scope) args )
  | Lambda (loc, xs, e) ->
      (* We ignore the type transformer since it is only relevant in 'let' bindings.
         See Note [PatternTypeTransformers] *)
      let xs', scope_trans, _ty_trans = rename_patterns scope xs in
      Lambda (loc, xs', rename_expr exports (scope_trans scope) e)
  | StringLit (loc, s) -> StringLit (loc, s)
  | NumLit (loc, n) -> NumLit (loc, n)
  | BoolLit (loc, b) -> BoolLit (loc, b)
  | UnitLit loc -> UnitLit loc
  | ListLit (loc, exprs) ->
      ListLit (loc, List.map (rename_expr exports scope) exprs)
  | TupleLit (loc, exprs) ->
      TupleLit (loc, List.map (rename_expr exports scope) exprs)
  | RecordLit (loc, kvs) ->
      RecordLit
        (loc, List.map (fun (k, e) -> (k, rename_expr exports scope e)) kvs)
  | StringInterpolation (loc, components) ->
      let rename_component = function
        | Parsed.StringComponent (loc, str) -> Renamed.StringComponent (loc, str)
        | Interpolation (loc, exprs) ->
            Interpolation (loc, rename_seq exports scope exprs)
      in
      let components = List.map rename_component components in
      StringInterpolation (loc, components)
  (* TODO: What about nested module subscripts? *)
  | ModSubscript (loc, mod_name, key) ->
      let mod_name, module_export_scope =
        RenameScope.lookup_mod_var scope loc mod_name
      in

      let key_name = RenameScope.lookup_var module_export_scope loc key in
      ModSubscript (loc, mod_name, key_name)
  | Subscript (loc, expr, key) ->
      Subscript (loc, rename_expr exports scope expr, key)
  | RecordUpdate (loc, expr, kvs) ->
      let rec duplicate_key previous = function
        | [] -> None
        | (key, _) :: rest when StringSet.mem key previous -> Some key
        | (key, _) :: rest -> duplicate_key (StringSet.add key previous) rest
      in
      begin
        match duplicate_key StringSet.empty kvs with
        | Some key ->
            raise (RenameError (DuplicateKeyInRecordUpdate (key, loc)))
        | None ->
            RecordUpdate
              ( loc,
                rename_expr exports scope expr,
                List.map
                  (fun (x, expr) -> (x, rename_expr exports scope expr))
                  kvs )
      end
  | RecordExtension (loc, expr, kvs) ->
      RecordExtension
        ( loc,
          rename_expr exports scope expr,
          List.map (fun (x, expr) -> (x, rename_expr exports scope expr)) kvs )
  | DynLookup (loc, mexpr, kexpr) ->
      DynLookup
        (loc, rename_expr exports scope mexpr, rename_expr exports scope kexpr)
  | BinOp (loc, e1, op, e2) ->
      BinOp
        ( loc,
          rename_expr exports scope e1,
          rename_binop op,
          rename_expr exports scope e2 )
  | Not (loc, expr) -> Not (loc, rename_expr exports scope expr)
  | Range (loc, start_expr, end_expr) ->
      Range
        ( loc,
          rename_expr exports scope start_expr,
          rename_expr exports scope end_expr )
  | ListComp (loc, result_expr, comp_exprs) ->
      let rec rename_comp scope renamed_comp_exprs_rev = function
        | [] ->
            Renamed.ListComp
              ( loc,
                rename_expr exports scope result_expr,
                List.rev renamed_comp_exprs_rev )
        | Parsed.FilterClause expr :: comps ->
            let expr' = rename_expr exports scope expr in
            rename_comp scope
              (FilterClause expr' :: renamed_comp_exprs_rev)
              comps
        | Parsed.DrawClause (pattern, expr) :: comps ->
            (* The expression is renamed with the previous scope, since
               draw clauses cannot be recursive *)
            let expr' = rename_expr exports scope expr in
            (* We don't need to (and probably shouldn't) use the type transformer here,
               (See Note [PatternTypeTransformers])
               Since expressions in draw clauses need to return lists, so the only way that this pattern
               could bind type variables would be if the list returned by `expr` had type `List(forall a. ...)`.

               This is impossible, since polaris does not support impredicative polymorphism.
            *)
            let pattern', scope_trans, _ty_trans =
              rename_pattern scope pattern
            in
            rename_comp (scope_trans scope)
              (DrawClause (pattern', expr') :: renamed_comp_exprs_rev)
              comps
      in
      rename_comp scope [] comp_exprs
  | If (loc, e1, e2, e3) ->
      If
        ( loc,
          rename_expr exports scope e1,
          rename_expr exports scope e2,
          rename_expr exports scope e3 )
  | Seq (loc, es) -> Seq (loc, rename_seq exports scope es)
  | LetSeq (loc, _, _)
  | LetRecSeq ({ main = loc; _ }, _, _, _, _)
  | LetEnvSeq (loc, _, _)
  (* TODO: Improve this error message *)
  | LetModuleSeq (loc, _, _)
  | LetDataSeq (loc, _, _, _)
  | LetTypeSeq (loc, _, _, _)
  | LetClassSeq (loc, _, _, _)
  | LetInstanceSeq (loc, _, _, _, _, _)
  | LetExceptionSeq (loc, _, _, _) ->
      raise (RenameError (LetSeqInNonSeq (expr, loc)))
  | ProgCall (loc, p, args) ->
      ProgCall (loc, p, List.map (rename_expr exports scope) args)
  | Pipe (loc, exprs) -> Pipe (loc, List.map (rename_expr exports scope) exprs)
  | EnvVar (loc, var) -> EnvVar (loc, var)
  | Async (loc, expr) -> Async (loc, rename_expr exports scope expr)
  | Await (loc, expr) -> Await (loc, rename_expr exports scope expr)
  | Match (loc, expr, branches) ->
      let expr' = rename_expr exports scope expr in
      let branches' =
        List.map
          (fun (pat, expr) ->
            (* The type transformer is only useful for the body of a definition so
               we ignore it here (See Note [PatternTypeTransformers]) *)
            let pat', scope_trans, _ty_trans = rename_pattern scope pat in
            let scope' = scope_trans scope in
            (pat', rename_expr exports scope' expr))
          branches
      in
      Match (loc, expr', branches')
  | Ascription (loc, expr, ty) ->
      let expr = rename_expr exports scope expr in
      let ty, _ = rename_type loc scope ty in
      Ascription (loc, expr, ty)
  | Unwrap (loc, expr) -> Unwrap (loc, rename_expr exports scope expr)
  | MakeRef (loc, expr) -> MakeRef (loc, rename_expr exports scope expr)
  | Assign (loc, place_expr, expr) ->
      let place_expr = rename_expr exports scope place_expr in
      let expr = rename_expr exports scope expr in
      Assign (loc, place_expr, expr)
  | Try (loc, try_expr, handlers) ->
      let try_expr = rename_expr exports scope try_expr in

      let rename_handler (pattern, expr) =
        let pattern, scope_transformer, _type_transformer =
          rename_pattern scope pattern
        in
        let expr = rename_expr exports (scope_transformer scope) expr in
        (pattern, expr)
      in

      let handlers = List.map rename_handler handlers in
      Try (loc, try_expr, handlers)
  | Raise (loc, expr) ->
      let expr = rename_expr exports scope expr in
      Raise (loc, expr)
  | ExprExt (_, void) -> .

and rename_seq_state
    (exports : (module_exports * Typed.expr list) FilePathMap.t)
    (scope : RenameScope.t) (exprs : Parsed.expr list) :
    Renamed.expr list * RenameScope.t =
  let open RenameScope in
  match exprs with
  | LetSeq (loc, p, e) :: exprs ->
      let p', scope_trans, ty_trans = rename_pattern scope p in
      (* Regular lets are non-recursive, so e' is *not* evaluated in the new scope.
         We still need to bind type variables in the inner scope though
         (See Note [PatternTypeTransformers] and the case for `Let`) *)
      let e' = rename_expr exports (ty_trans scope) e in
      let exprs', res_scope =
        rename_seq_state exports (scope_trans scope) exprs
      in
      (LetSeq (loc, p', e') :: exprs', res_scope)
  | LetRecSeq (loc, mty, x, patterns, e) :: exprs ->
      let x' = fresh_var x in
      let patterns', scope_trans, _param_ty_trans =
        rename_patterns scope patterns
      in
      let scope' = insert_var x x' scope in

      let mty', type_trans =
        match mty with
        | None -> (None, Fun.id)
        | Some ty ->
            let ty', ty_trans = rename_type loc.main scope ty in
            (Some ty', ty_trans)
      in
      let inner_scope = type_trans (scope_trans scope') in
      (* Let rec's *are* recursive! We should not apply the first type transformer since it
         only concerns the parameters, but we need to include the type transformer for the
         (potential) type annotation!
         (See Note [PatternTypeTransformers] and the case for `LetRec`) *)
      let e' = rename_expr exports inner_scope e in
      let exprs', res_scope = rename_seq_state exports scope' exprs in
      (LetRecSeq (loc, mty', x', patterns', e') :: exprs', res_scope)
  | LetEnvSeq (loc, x, e) :: exprs ->
      let e = rename_expr exports scope e in
      let exprs, scope = rename_seq_state exports scope exprs in
      (LetEnvSeq (loc, x, e) :: exprs, scope)
  | LetModuleSeq (loc, x, mod_expr) :: exprs ->
      let x' = fresh_var x in
      (* Module expressions are also non-recursive. Right now this is obviously the most
         sensible choice, but if we add functors, we might want to relax this restriction in the future *)
      let mod_expr, contents = rename_mod_expr exports scope mod_expr in
      let scope = insert_mod_var x x' contents scope in
      let exprs, scope = rename_seq_state exports scope exprs in
      (LetModuleSeq (loc, x', mod_expr) :: exprs, scope)
  | LetDataSeq (loc, data_name, params, ty) :: exprs ->
      let data_name' = fresh_var data_name in
      (* We insert the type constructor immediately to support recursion *)
      let scope =
        insert_type_constructor data_name data_name' (List.length params)
          DataConSort scope
      in

      let renamed_params =
        List.map
          (fun param ->
            let param' = fresh_var param in
            (param, param'))
          params
      in
      let type_scope =
        List.fold_right
          (fun (param, param') scope -> insert_type_var param param' scope)
          renamed_params scope
      in
      let ty', _ = rename_type loc type_scope ty in

      (* This uses 'scope' again since we really don't want bound type variables
         to bleed into the remaining expressions *)
      let scope =
        insert_data_constructor data_name data_name' NewtypeConSort scope
      in

      let exprs, scope = rename_seq_state exports scope exprs in

      ( LetDataSeq (loc, data_name', List.map snd renamed_params, ty') :: exprs,
        scope )
  | LetTypeSeq (loc, alias_name, params, underlying_type) :: exprs ->
      let alias_name' = fresh_var alias_name in

      let renamed_params =
        List.map
          (fun param ->
            let param' = fresh_var param in
            (param, param'))
          params
      in
      let type_scope =
        List.fold_right
          (fun (param, param') scope -> insert_type_var param param' scope)
          renamed_params scope
      in
      let underlying_type', _ = rename_type loc type_scope underlying_type in

      let scope =
        insert_type_constructor alias_name alias_name' (List.length params)
          TypeAliasSort
          (insert_type_alias alias_name'
             (List.map snd renamed_params)
             underlying_type' scope)
      in

      let exprs, scope = rename_seq_state exports scope exprs in
      ( LetTypeSeq
          (loc, alias_name', List.map snd renamed_params, underlying_type')
        :: exprs,
        scope )
  | LetClassSeq (loc, class_name, params, methods) :: exprs ->
      let class_name' = fresh_var class_name in
      let renamed_params =
        List.map
          (fun param ->
            let param' = fresh_var param in
            (param, param'))
          params
      in
      let method_scope =
        Util.compose
          (List.map
             (fun (param, param') scope -> insert_type_var param param' scope)
             renamed_params)
          scope
      in
      let methods' =
        List.map
          (fun (name, ty) ->
            let name' = fresh_var name in
            let ty', _ = rename_type loc method_scope ty in
            (name', ty'))
          methods
      in

      let method_map =
        StringMap.of_seq
          (Seq.map
             (fun (name, ty) -> (name.name, (name, ty)))
             (List.to_seq methods'))
      in

      let scope =
        Util.compose
          (List.map
             (fun (name, _) scope -> insert_var name.name name scope)
             methods')
          (insert_type_constructor class_name class_name' (List.length params)
             ClassSort
             (insert_type_class class_name'
                (List.map snd renamed_params)
                method_map scope))
      in

      let exprs, scope = rename_seq_state exports scope exprs in
      ( LetClassSeq (loc, class_name', List.map snd renamed_params, methods')
        :: exprs,
        scope )
  | LetInstanceSeq (loc, parameters, entailed, class_name, args, methods)
    :: exprs -> begin
      let add_type_variable scope variable =
        let renamed = fresh_var variable in
        (RenameScope.insert_type_var variable renamed scope, renamed)
      in

      let scope, parameters =
        List.fold_left_map add_type_variable scope parameters
      in

      let entailed, _ = rename_type loc scope entailed in

      let class_name', arg_count =
        match RenameMap.find_opt class_name scope.ty_constructors with
        | Some (class_name', arg_count, ClassSort)
          when List.compare_length_with args arg_count <> 0 ->
            raise
              (RenameError
                 (WrongNumberOfClassArgs
                    {
                      class_name = class_name';
                      expected = arg_count;
                      actual = List.length args;
                      loc;
                    }))
        | Some (class_name', arg_count, ClassSort) -> (class_name', arg_count)
        | Some (class_name', _, _) ->
            raise (RenameError (NonClassInInstance (class_name', loc)))
        | None ->
            raise_notrace (RenameError (DataConNotFound (class_name, loc)))
      in

      match NameMap.find_opt class_name' scope.type_classes with
      | None ->
          panic __LOC__
            "Type constructor with ClassSort but without entry in \
             scope.type_classes"
      | Some (_params, definition_methods) -> (
          let args = List.map (fst << rename_type loc scope) args in

          let invalid_methods, methods' =
            List.partition_map
              (fun ((), name, patterns, body) ->
                match StringMap.find_opt name definition_methods with
                | None -> Left name
                | Some (name', ty) ->
                    (* We can safely ignore the type transformer, since this is only relevant
                       in non-seq let bindings where the pattern describes what we are defining
                       (rather than a parameter like in this case) *)
                    let patterns, scope_trans, _type_trans =
                      rename_patterns scope patterns
                    in
                    let body = rename_expr exports (scope_trans scope) body in
                    Right (ty, name', patterns, body))
              methods
          in
          let method_names = List.map (fun (_, name, _, _) -> name) methods in
          let missing_methods =
            List.filter
              (fun name -> not (List.mem name method_names))
              (List.of_seq (Seq.map fst (StringMap.to_seq definition_methods)))
          in
          match (invalid_methods, missing_methods) with
          | [], [] ->
              let exprs, scope = rename_seq_state exports scope exprs in
              ( LetInstanceSeq
                  (loc, parameters, entailed, class_name', args, methods')
                :: exprs,
                scope )
          | _ ->
              raise
                (RenameError
                   (ClassMethodMismatch
                      {
                        class_name = class_name';
                        invalid = invalid_methods;
                        missing = missing_methods;
                        loc;
                      })))
    end
  | LetExceptionSeq (loc, exception_name, params, message_expr) :: exprs ->
      let rename_param scope (param_name, ty) =
        let param_name' = fresh_var param_name in
        let ty, _ty_transformer = rename_type loc scope ty in
        (insert_var param_name param_name' scope, (param_name', ty))
      in
      let message_scope, params =
        List.fold_left_map rename_param scope params
      in
      let message_expr = rename_expr exports message_scope message_expr in

      let exception_name' = fresh_var exception_name in

      let scope =
        insert_data_constructor exception_name exception_name' ExceptionSort
          scope
      in

      let exprs, scope = rename_seq_state exports scope exprs in
      ( LetExceptionSeq (loc, exception_name', params, message_expr) :: exprs,
        scope )
  | e :: exprs ->
      let e' = rename_expr exports scope e in
      let exprs', res_state = rename_seq_state exports scope exprs in
      (e' :: exprs', res_state)
  | [] -> ([], scope)

and rename_seq exports scope exprs =
  let res, _ = rename_seq_state exports scope exprs in
  res

let rename_option (scope : RenameScope.t) (flag_def : Parsed.flag_def) :
    Renamed.flag_def * RenameScope.t =
  let args, scope =
    match flag_def.args with
    | Varargs name ->
        let name' = fresh_var name in
        (Renamed.Varargs name', RenameScope.insert_var name name' scope)
    | Switch name ->
        let name' = fresh_var name in
        (Renamed.Switch name', RenameScope.insert_var name name' scope)
    | Named args ->
        let args' = List.map fresh_var args in
        let scope = List.fold_right2 RenameScope.insert_var args args' scope in
        (Named args', scope)
    | NamedDefault args ->
        let args' = List.map (fun (x, def) -> (fresh_var x, def)) args in
        let scope =
          List.fold_right2
            (fun (x, _) (y, _) -> RenameScope.insert_var x y)
            args args' scope
        in
        (NamedDefault args', scope)
  in
  ({ args; flags = flag_def.flags; description = flag_def.description }, scope)

let rename_exports :
    RenameScope.t -> Parsed.export_item list -> Renamed.export_item list =
 fun scope ->
  List.map
    begin
      function
      | Parsed.ExportVal (loc, name) ->
          Renamed.ExportVal (loc, RenameScope.lookup_var scope loc name)
      | Parsed.ExportConstructor (loc, name) -> (
          match RenameMap.find_opt name scope.ty_constructors with
          | Some (renamed, _, _sort) -> begin
              match NameMap.find_opt renamed scope.type_classes with
              | Some (params, fields) ->
                  Renamed.ExportConstructor
                    ( ( loc,
                        `Class (params, List.map snd (StringMap.to_list fields))
                      ),
                      renamed )
              | None -> Renamed.ExportConstructor ((loc, `Type), renamed)
            end
          | None -> (
              match RenameMap.find_opt name scope.data_constructors with
              | Some (name, ExceptionSort) ->
                  Renamed.ExportConstructor ((loc, `Exception), name)
              | _ -> raise (RenameError (UnboundExportConstructor (name, loc))))
          )
    end

let rename_scope
    (exports : (Typed.module_exports * Typed.expr list) FilePathMap.t)
    (scope : RenameScope.t) (header : Parsed.header) (exprs : Parsed.expr list)
    : Renamed.header * Renamed.expr list * RenameScope.t =
  let rec go scope = function
    | flag_def :: defs ->
        let flag_def, scope = rename_option scope flag_def in
        let defs, scope = go scope defs in
        (flag_def :: defs, scope)
    | [] -> ([], scope)
  in
  let options, scope = go scope header.options in

  (* We need to rename the body before finishing the header, since
     the export list depends on names bound in the body *)
  let exprs', scope_after_body = rename_seq_state exports scope exprs in

  ( {
      usage = header.usage;
      description = header.description;
      exports = rename_exports scope_after_body header.exports;
      options;
    },
    exprs',
    scope_after_body )
