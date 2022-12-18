open Syntax
open Util

module RenameMap = Map.Make(String)

module FilePathMap = Map.Make(String)

module RenameError = struct
    exception VarNotFound of string * loc
    exception ModuleVarNotFound of string * loc
    exception SubscriptVarNotFound of string * loc
    exception LetSeqInNonSeq of Parsed.expr * loc
    exception SubModuleNotFound of string * loc
end

module RenameScope = struct
    open RenameMap
    type t = { 
        variables: name RenameMap.t;
        module_vars: (name * t) RenameMap.t;
    }

    let empty : t = { 
            variables = RenameMap.empty; 
            module_vars = RenameMap.empty;
        }

    let insert_var (old : string) (renamed : name) (scope : t) : t =
        { scope with variables = add old renamed scope.variables }
    
    let insert_mod_var (old : string) (renamed : name) (contents : t) (scope : t) : t =
        { scope with module_vars = add old (renamed, contents) scope.module_vars }

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

let rec rename_pattern (scope : RenameScope.t) = let open RenameScope in function
    | Parsed.VarPat (loc, var) ->
        let var' = fresh_var var in
        (Renamed.VarPat (loc, var'), fun scope -> insert_var var var' scope)
    | ConsPat (loc, x, xs) ->
        let x', x_trans = rename_pattern scope x in
        let xs', xs_trans = rename_pattern scope xs in
        (ConsPat (loc, x', xs'), fun scope -> xs_trans (x_trans scope))
    | ListPat (loc, pats) ->
        let pats', pats_trans = List.split (List.map (rename_pattern scope) pats) in
        (ListPat (loc, pats'), List.fold_right (fun t r x -> t (r x)) pats_trans (fun x -> x))
    | TuplePat (loc, pats) ->
        let pats', pats_trans = List.split (List.map (rename_pattern scope) pats) in
        (TuplePat (loc, pats'), List.fold_right (fun t r x -> t (r x)) pats_trans (fun x -> x))    
    | NumPat (loc, f) ->
        (NumPat (loc, f), fun x -> x)
    | OrPat(loc, p1, p2) ->
        let p1', p1_trans = rename_pattern scope p1 in
        let p2', p2_trans = rename_pattern scope p2 in
        (OrPat(loc, p1', p2'), fun scope -> p2_trans (p1_trans scope))

let rename_patterns scope pats =
    List. fold_right (fun pat (pats', trans) -> begin
        let pat', pat_trans = rename_pattern scope pat in
        (pat' :: pats', fun s -> pat_trans (trans s))
    end) pats ([], fun x -> x)

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
    | App (loc, f, args) -> App (loc, rename_expr exports scope f, List.map (rename_expr exports scope) args)
    
    | Lambda (loc, xs, e) ->
        let xs', scope_trans = rename_patterns scope xs in
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
    | Subscript (loc, Var (var_loc, name), key) ->
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
            let pattern', scope_trans = rename_pattern scope pattern in
            rename_comp (scope_trans scope) (DrawClause (pattern', expr') :: renamed_comp_exprs_rev) comps
        in
        rename_comp scope [] comp_exprs

    | If(loc, e1, e2, e3) -> If(loc, rename_expr exports scope e1, rename_expr exports scope e2, rename_expr exports scope e3)

    | Seq (loc, es) -> Seq (loc, rename_seq exports scope es)

    | LetSeq (loc, _, _) | LetRecSeq (loc, _, _, _) | LetEnvSeq (loc, _, _) 
    | LetModuleSeq (loc, _, _) -> raise (RenameError.LetSeqInNonSeq (expr, loc))

    | Let (loc, p, e1, e2) ->
        let p', scope_trans = rename_pattern scope p in
        (* regular lets are non-recursive, so e1 is *not* evaluated in the new scope *)
        Let (loc, p', rename_expr exports scope e1, rename_expr exports (scope_trans scope) e2)
    | LetRec (loc, x, patterns, e1, e2) ->
        let x' = fresh_var x in
        let patterns', scope_trans = rename_patterns scope patterns in
        let scope' = insert_var x x' scope in
        let inner_scope = scope_trans scope' in
        (* let rec's *are* recursive *)
        LetRec(loc, x', patterns', rename_expr exports inner_scope e1, rename_expr exports scope' e2)
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
                let pat', scope_trans = rename_pattern scope pat in
                let scope' = scope_trans scope in
                (pat', rename_expr exports scope' expr)
            ) branches 
        in
        Match(loc, expr', branches')

and rename_seq_state (exports : (module_exports * Renamed.expr list) FilePathMap.t) (scope : RenameScope.t) (exprs : Parsed.expr list) : Renamed.expr list * RenameScope.t = 
    let open RenameScope in
    match exprs with
    | (LetSeq (loc, p, e) :: exprs) -> 
        let p', scope_trans = rename_pattern scope p in
        (* regular lets are non-recursive, so e' is *not* evaluated in the new scope *)
        let e' = rename_expr exports scope e in
        let exprs', res_scope = rename_seq_state exports (scope_trans scope) exprs in
        (LetSeq (loc, p', e') :: exprs', res_scope)
    | LetRecSeq (loc, x, patterns, e) :: exprs -> 
        let x' = fresh_var x in
        let patterns', scope_trans = rename_patterns scope patterns in
        let scope' = insert_var x x' scope in
        let inner_scope = scope_trans scope' in
        (* let rec's *are* recursive! *)
        let e' = rename_expr exports inner_scope e in
        let exprs', res_scope = rename_seq_state exports scope' exprs in
        (LetRecSeq(loc, x', patterns', e') :: exprs', res_scope)
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

