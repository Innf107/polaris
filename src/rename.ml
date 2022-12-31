open Syntax
open Util

module RenameMap = Map.Make(String)

module FilePathMap = Map.Make(String)

module RenameError = struct
    exception VarNotFound of string * loc
    exception ModuleVarNotFound of string * loc
    exception TyVarNotFound of string * loc
    exception TyConNotFound of string * loc
    exception SubscriptVarNotFound of string * loc
    exception LetSeqInNonSeq of Parsed.expr * loc
    exception SubModuleNotFound of string * loc
    exception HigherRankType of Parsed.ty * loc
    exception WrongNumberOfTyConArgs of name * int * Parsed.ty list * loc
end

module RenameScope = struct
    open RenameMap
    type t = { 
        variables: name RenameMap.t;
        module_vars: (name * t) RenameMap.t;
        ty_vars: name RenameMap.t;
        (* Polaris does not have a Haskell-style kind system, so we
           just check that type constructors are always fully applied in the renamer. *)
        ty_constructors: (name * int) RenameMap.t;
        data_constructors: name RenameMap.t;
    }

    let empty : t = { 
            variables = RenameMap.empty; 
            module_vars = RenameMap.empty;
            ty_vars = RenameMap.empty;
            ty_constructors = RenameMap.empty;
            data_constructors = RenameMap.empty;
        }

    let insert_var (old : string) (renamed : name) (scope : t) : t =
        { scope with variables = add old renamed scope.variables }

    let insert_mod_var (old : string) (renamed : name) (contents : t) (scope : t) : t =
        { scope with module_vars = add old (renamed, contents) scope.module_vars }

    let insert_type_var (old : string) (renamed : name) (scope : t) : t =
        { scope with ty_vars = add old renamed scope.ty_vars }    

    let insert_type_constructor old renamed arg_count scope =
        { scope with ty_constructors = add old (renamed, arg_count) scope.ty_constructors }

    let insert_data_constructor old renamed scope =
        { scope with data_constructors = add old renamed scope.data_constructors }    

    let lookup_var (scope : t) (loc : loc) (var : string) : name =
        try 
            find var scope.variables 
        with
            Not_found -> raise (RenameError.VarNotFound (var, loc))

    let lookup_mod_var (scope : t) (loc : loc) (var : string) : name * t =
        try
            find var scope.module_vars
        with
            Not_found -> raise (RenameError.ModuleVarNotFound (var, loc))
end

let fresh_var = Name.fresh

let rename_type loc (scope : RenameScope.t) original_type = 
    let rec go (scope : RenameScope.t) ty = 
        (* Polaris does not support higher rank, let alone impredicative polymorphism,
        so top level foralls are the only way to bind type variables and everything else
        can use its own case *)
        let rec rename_nonbinding = function
        | Parsed.Number -> Renamed.Number
        | Bool -> Bool
        | String -> String
        | List(ty) -> List(rename_nonbinding ty)
        | Promise(ty) -> Promise(rename_nonbinding ty)
        | Tuple(tys) -> Tuple(Array.map rename_nonbinding tys)
        | Fun(tys1, ty) -> Fun(List.map rename_nonbinding tys1, rename_nonbinding ty)
        | Record(RowClosed tys) -> Record(RowClosed (Array.map (fun (x, ty) -> (x, rename_nonbinding ty)) tys))
        | Record(RowVar (tys, varname)) -> 
            begin match RenameMap.find_opt varname scope.ty_vars with
            | Some varname -> Record(RowVar (Array.map (fun (x, ty) -> (x, rename_nonbinding ty)) tys, varname))
            | None -> raise (RenameError.TyVarNotFound(varname, loc))
            end
        | TyConstructor(name, args) ->
            let name, arg_count = match RenameMap.find_opt name scope.ty_constructors with
            | Some (name, arg_count) -> (name, arg_count)
            | None -> raise (RenameError.TyConNotFound(name, loc))
            in
            if List.compare_length_with args arg_count <> 0 then begin
                raise (RenameError.WrongNumberOfTyConArgs(name, arg_count, args, loc))
            end;
            let args = List.map rename_nonbinding args in
            TyConstructor(name, args)
        | TyVar(tv) -> begin match RenameMap.find_opt tv scope.ty_vars with
            | Some tv' -> TyVar(tv')
            | None -> match RenameMap.find_opt tv scope.ty_constructors with
                | Some (name, 0) -> TyConstructor(name, [])
                | Some (name, arg_count) -> raise (RenameError.WrongNumberOfTyConArgs(name, arg_count, [], loc))
                | None -> raise (RenameError.TyVarNotFound(tv, loc))
            end
        | Parsed.Forall _ -> raise (RenameError.HigherRankType(ty, loc))
        | Unif(_) -> panic __LOC__ ("Unification variable found after parsing. How did this happen wtf?")
        | Skol(_) -> panic __LOC__ ("Skolem found after parsing. How did this happen wtf?")
        | Record(RowUnif _) -> panic __LOC__ ("Unification variable record found after parsing. How did this happen wtf?")
        | Record(RowSkol _) -> panic __LOC__ ("Skolem record found after parsing. How did this happen wtf?")
        in
        match ty with
        | Parsed.Forall(tv, ty) ->
            let tv' = fresh_var tv in
            let scope_trans = RenameScope.insert_type_var tv tv' in
            let ty', other_trans = go (scope_trans scope) ty in
            Renamed.Forall(tv', ty'), (fun scope -> scope_trans (other_trans scope))
        | _ -> rename_nonbinding ty, Fun.id
    in go scope original_type


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
let rec rename_pattern (scope : RenameScope.t) = let open RenameScope in function
    | Parsed.VarPat (loc, var) ->
        let var' = fresh_var var in
        (Renamed.VarPat (loc, var'), (fun scope -> insert_var var var' scope), fun x -> x)
    | ConsPat (loc, x, xs) ->
        let x', x_trans, x_ty_trans = rename_pattern scope x in
        let xs', xs_trans, xs_ty_trans = rename_pattern scope xs in
        ( ConsPat (loc, x', xs')
        , (fun scope -> xs_trans (x_trans scope))
        , (fun scope -> xs_ty_trans (x_ty_trans scope))
        )
    | ListPat (loc, pats) ->
        let pats', pats_trans, pats_ty_trans = Util.split3 (List.map (rename_pattern scope) pats) in
        ( ListPat (loc, pats')
        , Util.compose pats_trans
        , Util.compose pats_ty_trans
        )
    | TuplePat (loc, pats) ->
        let pats', pats_trans, pats_ty_trans = Util.split3 (List.map (rename_pattern scope) pats) in
        ( TuplePat (loc, pats')
        , Util.compose pats_trans
        , Util.compose pats_ty_trans
        )    
    | NumPat (loc, f) ->
        (NumPat (loc, f)
        , (fun x -> x)
        , (fun x -> x)
        )
    | OrPat(loc, p1, p2) ->
        let p1', p1_trans, p1_ty_trans = rename_pattern scope p1 in
        let p2', p2_trans, p2_ty_trans = rename_pattern scope p2 in
        ( OrPat(loc, p1', p2')
        , (fun scope -> p2_trans (p1_trans scope))
        , (fun scope -> p2_ty_trans (p1_ty_trans scope))
        )
    | TypePat (loc, p, ty) ->
        let p', p_trans, p_ty_trans = rename_pattern scope p in
        let ty', ty_trans = rename_type loc scope ty in
        ( TypePat (loc, p', ty')
        , p_trans
        , (fun scope -> ty_trans (p_ty_trans scope))
        )
    | DataPat(loc, constructor_name, pattern) ->
        let constructor_name = match RenameMap.find_opt constructor_name scope.data_constructors with
        | Some name -> name
        | None -> panic __LOC__ (Loc.pretty loc ^ ": Polymorphic variants are not implemented yet! Maybe you misspelled a data constructor?")
        in
        let patterns, scope_transformer, type_transformer = rename_pattern scope pattern in
        ( DataPat(loc, constructor_name, patterns)
        , scope_transformer
        , type_transformer
        )
        

let rename_patterns scope pats =
    List. fold_right (fun pat (pats', trans, ty_trans) -> begin
        let pat', pat_trans, pat_ty_trans = rename_pattern scope pat in
        (pat' :: pats', (fun scope -> pat_trans (trans scope)), fun scope -> pat_ty_trans (ty_trans scope))
    end) pats ([], (fun x -> x), (fun x -> x))

let rec rename_mod_expr : (module_exports * Renamed.expr list) FilePathMap.t
                   -> RenameScope.t 
                   -> Parsed.module_expr 
                   -> Renamed.module_expr * RenameScope.t
= fun exports scope -> function
    | ModVar (loc, mod_var) -> 
        let name, contents = RenameScope.lookup_mod_var scope loc mod_var in
        ModVar(loc, name), contents
    | Import (loc, path) -> begin match FilePathMap.find_opt path exports with
        | None -> panic __LOC__ ("import path not found in renamer: '" ^ path ^ "'.\nImports: [" ^ String.concat ", " (List.map fst (FilePathMap.bindings exports)) ^ "]")
        | Some (mod_exports, body) ->
            let scope = 
                StringMap.fold 
                    (fun name renamed r -> RenameScope.insert_var name renamed  r) 
                    mod_exports.exported_names 
                    RenameScope.empty
            in
            Import ((loc, mod_exports, body), path), scope
        end
    | SubModule (loc, mod_expr, field) ->
        let mod_expr', contents = rename_mod_expr exports scope mod_expr in
        let field', sub_contents = match StringMap.find_opt field contents.module_vars with
        | None -> raise (RenameError.SubModuleNotFound (field, loc))
        | Some (field', sub_contents) -> field', sub_contents                                     
        in
        SubModule (loc, mod_expr', field'), sub_contents

let rename_binop : Parsed.binop -> Renamed.binop =
    function
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

let rec rename_expr (exports : (module_exports * Renamed.expr list) FilePathMap.t) (scope : RenameScope.t) (expr : Parsed.expr): Renamed.expr = let open RenameScope in
    match expr with 
    | Var (loc, var_name) ->
        if Primops.is_primop var_name
        then Var(loc, {name=var_name; index=Name.primop_index})
        else Var (loc, lookup_var scope loc var_name)
    | DataConstructor (loc, constructor_name) ->
        begin match RenameMap.find_opt constructor_name scope.data_constructors with
        | Some constructor_name ->
            DataConstructor (loc, constructor_name)
        | None ->
            (* TODO: With polymorphic variants this should return a 'VariantConstructor' *)
            panic __LOC__ (Loc.pretty loc ^ ": Polymorphic variants are not implemented yet! Maybe you misspelled a data constructor?")
        end
    | App (loc, f, args) -> App (loc, rename_expr exports scope f, List.map (rename_expr exports scope) args)
    
    | Lambda (loc, xs, e) ->
        (* We ignore the type transformer since it is only relevant in 'let' bindings. 
           See Note [PatternTypeTransformers] *)
        let xs', scope_trans, _ty_trans = rename_patterns scope xs in
        Lambda (loc, xs', rename_expr exports (scope_trans scope) e)

    | StringLit (loc, s) -> StringLit (loc, s)
    | NumLit (loc, n) -> NumLit (loc, n)
    | BoolLit (loc, b) -> BoolLit (loc, b)
    | UnitLit loc -> UnitLit loc
    | NullLit loc -> NullLit loc
    | ListLit (loc, exprs) -> ListLit (loc, List.map (rename_expr exports scope) exprs)
    | TupleLit (loc, exprs) -> TupleLit (loc, List.map (rename_expr exports scope) exprs)
    | RecordLit (loc, kvs) -> RecordLit (loc, List.map (fun (k, e) -> (k, rename_expr exports scope e)) kvs)

    (* TODO: What about nested module subscripts? *)
    | Subscript (loc, (Var (var_loc, name) | DataConstructor(var_loc, name)), key) ->
        begin match RenameScope.lookup_var scope var_loc name with
        | exception (RenameError.VarNotFound _) -> 
            begin match RenameScope.lookup_mod_var scope var_loc name with
            | exception (RenameError.ModuleVarNotFound _) -> raise (RenameError.SubscriptVarNotFound (name, loc))
            | (renamed, module_export_scope) ->
                (* TODO: Include the fact that this is looked up in another module in the error message! *)
                let key_name = RenameScope.lookup_var module_export_scope loc key in
                ModSubscript (loc, renamed, key_name)
            end
        | new_name -> Subscript (loc, Var (var_loc, new_name), key)
        end
    | Subscript (loc, expr, key) -> Subscript (loc, rename_expr exports scope expr, key)
    | ModSubscript (void, _, _) -> absurd void
    | RecordUpdate (loc, expr, kvs) -> RecordUpdate (loc, rename_expr exports scope expr, List.map (fun (x, expr) -> (x, rename_expr exports scope expr)) kvs)
    | RecordExtension (loc, expr, kvs) -> RecordExtension (loc, rename_expr exports scope expr, List.map (fun (x, expr) -> (x, rename_expr exports scope expr)) kvs)
    | DynLookup (loc, mexpr, kexpr) -> DynLookup (loc, rename_expr exports scope mexpr, rename_expr exports scope kexpr)

    | BinOp(loc, e1, op, e2) -> BinOp(loc, rename_expr exports scope e1, rename_binop op, rename_expr exports scope e2)
    | Not(loc, expr)     -> Not(loc, rename_expr exports scope expr)

    | Range(loc, start_expr, end_expr) -> Range(loc, rename_expr exports scope start_expr, rename_expr exports scope end_expr)
    | ListComp(loc, result_expr, comp_exprs) ->
        let rec rename_comp scope renamed_comp_exprs_rev = function
        | [] -> Renamed.ListComp(loc, rename_expr exports scope result_expr, List.rev renamed_comp_exprs_rev)
        | Parsed.FilterClause expr :: comps ->
            let expr' = rename_expr exports scope expr in
            rename_comp scope (FilterClause expr' :: renamed_comp_exprs_rev) comps
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
            let pattern', scope_trans, _ty_trans = rename_pattern scope pattern in
            rename_comp (scope_trans scope) (DrawClause (pattern', expr') :: renamed_comp_exprs_rev) comps
        in
        rename_comp scope [] comp_exprs

    | If(loc, e1, e2, e3) -> If(loc, rename_expr exports scope e1, rename_expr exports scope e2, rename_expr exports scope e3)

    | Seq (loc, es) -> Seq (loc, rename_seq exports scope es)

    | LetSeq (loc, _, _) | LetRecSeq (loc, _, _, _, _) | LetEnvSeq (loc, _, _) 
    | LetModuleSeq (loc, _, _) | LetDataSeq(loc, _, _, _) -> raise (RenameError.LetSeqInNonSeq (expr, loc))

    | Let (loc, p, e1, e2) ->
        let p', scope_trans, ty_trans = rename_pattern scope p in
        (* Regular lets are non-recursive, so e1 is *not* evaluated in the new scope with the value transformer applied.
           Let patterns *can* bind type variables though (See Note [PatternTypeTransformers]), so `e1` needs
           to be evaluated in a scope where `ty_trans` has been applied.

           At the same time, `e2` should *not* be able to use the type variables bound by `p` *)
        Let (loc, p', rename_expr exports (ty_trans scope) e1, rename_expr exports (scope_trans scope) e2)
    | LetRec (loc, mty, x, patterns, e1, e2) ->
        let x' = fresh_var x in
        let patterns', scope_trans, _param_ty_trans = rename_patterns scope patterns in
        let scope' = insert_var x x' scope in

        let mty', type_trans = match mty with
        | None -> None, Fun.id
        | Some ty ->
            let ty', ty_trans = rename_type loc scope ty in
            Some ty', ty_trans
        in

        let inner_scope = type_trans (scope_trans scope') in

        (* let rec's *are* recursive. The fist type transformer is part of the *parameters*, so we ignore it here. 
           We do need to include the type transformer for the (possible) type annotation though!
           (See Note [PatternTypeTransformers]) *)
        LetRec(loc, mty', x', patterns', rename_expr exports inner_scope e1, rename_expr exports scope' e2)
    | LetEnv (loc, x, e1, e2) ->
        LetEnv (loc, x, rename_expr exports scope e1, rename_expr exports scope e2)
    | Assign (loc, x, e) ->
        let x' = lookup_var scope loc x in
        Assign (loc, x', rename_expr exports scope e)

    | ProgCall (loc, p, args) ->
        ProgCall (loc, p, List.map (rename_expr exports scope) args)
    | Pipe (loc, exprs) ->
        Pipe (loc, List.map (rename_expr exports scope) exprs)
    | EnvVar (loc, var) -> EnvVar(loc, var)
    | Async (loc, expr) ->
        Async (loc, rename_expr exports scope expr)
    | Await (loc, expr) ->
        Await (loc, rename_expr exports scope expr)
    | Match (loc, expr, branches) ->
        let expr' = rename_expr exports scope expr in
        let branches' = List.map (
            fun (pat, expr) -> 
                (* The type transformer is only useful for the body of a definition so
                   we ignore it here (See Note [PatternTypeTransformers]) *)
                let pat', scope_trans, _ty_trans = rename_pattern scope pat in
                let scope' = scope_trans scope in
                (pat', rename_expr exports scope' expr)
            ) branches 
        in
        Match(loc, expr', branches')
    | Ascription (loc, expr, ty) ->
        let expr = rename_expr exports scope expr in
        let ty, _ = rename_type loc scope ty in
        Ascription (loc, expr, ty)

and rename_seq_state (exports : (module_exports * Renamed.expr list) FilePathMap.t) (scope : RenameScope.t) (exprs : Parsed.expr list) : Renamed.expr list * RenameScope.t = 
    let open RenameScope in
    match exprs with
    | (LetSeq (loc, p, e) :: exprs) -> 
        let p', scope_trans, ty_trans = rename_pattern scope p in
        (* Regular lets are non-recursive, so e' is *not* evaluated in the new scope.
           We still need to bind type variables in the inner scope though 
           (See Note [PatternTypeTransformers] and the case for `Let`) *)
        let e' = rename_expr exports (ty_trans scope) e in
        let exprs', res_scope = rename_seq_state exports (scope_trans scope) exprs in
        (LetSeq (loc, p', e') :: exprs', res_scope)
    | LetRecSeq (loc, mty, x, patterns, e) :: exprs -> 
        let x' = fresh_var x in
        let patterns', scope_trans, _param_ty_trans = rename_patterns scope patterns in
        let scope' = insert_var x x' scope in

        let mty', type_trans = match mty with
        | None -> None, Fun.id
        | Some ty ->
            let ty', ty_trans = rename_type loc scope ty in
            Some ty', ty_trans
        in
        let inner_scope = type_trans (scope_trans scope') in
        (* Let rec's *are* recursive! We should not apply the first type transformer since it
           only concerns the parameters, but we need to include the type transformer for the
           (potential) type annotation!
           (See Note [PatternTypeTransformers] and the case for `LetRec`) *)
        let e' = rename_expr exports inner_scope e in
        let exprs', res_scope = rename_seq_state exports scope' exprs in
        (LetRecSeq(loc, mty', x', patterns', e') :: exprs', res_scope)
    | LetEnvSeq (loc, x, e) :: exprs ->
        let e = rename_expr exports scope e in
        let exprs, scope = rename_seq_state exports scope exprs in
        LetEnvSeq (loc, x, e) :: exprs, scope
    | LetModuleSeq (loc, x, mod_expr) :: exprs ->
        let x' = fresh_var x in
        (* Module expressions are also non-recursive. Right now this is obviously the most
           sensible choice, but if we add functors, we might want to relax this restriction in the future *)
        let mod_expr, contents = rename_mod_expr exports scope mod_expr in
        let scope = insert_mod_var x x' contents scope in
        let exprs, scope = rename_seq_state exports scope exprs in
        LetModuleSeq (loc, x', mod_expr) :: exprs, scope
    | LetDataSeq (loc, data_name, params, ty) :: exprs ->
        let data_name' = fresh_var data_name in
        (* We insert the type constructor immediately to support recursion *)
        let scope = insert_type_constructor data_name data_name' (List.length params) scope in
        
        let renamed_params = List.map (fun param ->
            let param' = fresh_var param in
            (param, param')) params 
        in
        let type_scope = List.fold_right 
            (fun (param, param') scope -> insert_type_var param param' scope) 
            renamed_params 
            scope 
        in
        let ty', _ = rename_type loc type_scope ty in

        (* This uses 'scope' again since we really don't want bound type variables
           to bleed into the remaining expressions *)
        let scope = insert_data_constructor data_name data_name' scope in

        let exprs, scope = rename_seq_state exports scope exprs in

        LetDataSeq (loc, data_name', List.map snd renamed_params, ty') :: exprs, scope
    | (e :: exprs) -> 
        let e' = rename_expr exports scope e in
        let exprs', res_state = rename_seq_state exports scope exprs in
        (e' :: exprs', res_state)
    | [] -> ([], scope)
and rename_seq exports scope exprs =
    let res, _ = rename_seq_state exports scope exprs in
    res

let rename_option (scope : RenameScope.t) (flag_def : Parsed.flag_def): Renamed.flag_def * RenameScope.t =
    let args, scope = match flag_def.args with
        | Varargs name -> 
            let name' = fresh_var name in
            Renamed.Varargs name', RenameScope.insert_var name name' scope
        | Switch name ->
            let name' = fresh_var name in
            Renamed.Switch name', RenameScope.insert_var name name' scope
        | Named args ->
            let args' = List.map fresh_var args in
            let scope = List.fold_right2 (RenameScope.insert_var) args args' scope in
            Named args', scope
        | NamedDefault args ->
            let args' = List.map (fun (x, def) -> (fresh_var x, def)) args in
            let scope = List.fold_right2 (fun (x, _) (y, _) -> RenameScope.insert_var x y) args args' scope in
            NamedDefault args', scope
    in        
    { args
    ; flags = flag_def.flags
    ; description = flag_def.description
    }, scope

let rename_exports : RenameScope.t -> Parsed.export_item list -> Renamed.export_item list = 
    fun scope -> List.map begin function
        (Parsed.ExportVal (loc, name)) -> Renamed.ExportVal (loc, RenameScope.lookup_var scope loc name)
    end
    

let rename_scope (exports : (module_exports * Renamed.expr list) FilePathMap.t) (scope : RenameScope.t) (header : Parsed.header) (exprs : Parsed.expr list): Renamed.header * Renamed.expr list * RenameScope.t =
    let rec go scope = function
    | (flag_def::defs) ->
        let flag_def, scope = rename_option scope flag_def in
        let defs, scope = go scope defs in
        flag_def :: defs, scope
    | [] -> [], scope
    in
    let options, scope = go scope header.options in

    (* We need to rename the body before finishing the header, since
       the export list depends on names bound in the body *)
    let exprs', scope_after_body = rename_seq_state exports scope exprs in

    { usage = header.usage
    ; description = header.description
    ; exports = rename_exports scope_after_body header.exports
    ; options
    }, exprs', scope_after_body

