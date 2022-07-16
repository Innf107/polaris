open Syntax
open Util

module RenameMap = Map.Make(String)

module RenameError = struct
    exception VarNotFound of string * loc
    exception LetSeqInNonSeq of Parsed.expr * loc
end

module RenameScope = struct
    open RenameMap
    type t = {counter: int ref; variables: name RenameMap.t}

    let empty : t =
        { counter = ref 0; variables = RenameMap.empty }

    let insert_var (old : string) (renamed : name) (scope : t) : t =
        { scope with variables = add old renamed scope.variables }
    
    let lookup_var (scope : t) (loc : loc) (var : string) : 'name =
        try 
            find var scope.variables 
        with
            Not_found -> raise (RenameError.VarNotFound (var, loc))

    let fresh_var (scope : t) (var : string) : 'name =
        let ix = !(scope.counter) in
        scope.counter := ix + 1;
        { name = var; index = ix }
end

let rec rename_pattern (scope : RenameScope.t) = let open RenameScope in function
    | Parsed.VarPat (loc, var) ->
        let var' = fresh_var scope var in
        (Renamed.VarPat (loc, var'), fun scope -> insert_var var var' scope)
    | ConsPat (loc, x, xs) ->
        let x', x_trans = rename_pattern scope x in
        let xs', xs_trans = rename_pattern scope xs in
        (ConsPat (loc, x', xs'), fun scope -> xs_trans (x_trans scope))
    | ListPat (loc, pats) ->
        let pats', pats_trans = List.split (List.map (rename_pattern scope) pats) in
        (ListPat (loc, pats'), List.fold_right (fun t r x -> t (r x)) pats_trans (fun x -> x))
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

let rec rename_expr (scope : RenameScope.t) (expr : Parsed.expr): Renamed.expr = let open RenameScope in
    match expr with 
    | Var (loc, var_name) ->
        if Primops.is_primop var_name
        then Var(loc, {name=var_name; index= -1})
        else Var (loc, lookup_var scope loc var_name)
    | App (loc, f, args) -> App (loc, rename_expr scope f, List.map (rename_expr scope) args)
    
    | Lambda (loc, xs, e) ->
        let xs', scope_trans = rename_patterns scope xs in
        Lambda (loc, xs', rename_expr (scope_trans scope) e)

    | StringLit (loc, s) -> StringLit (loc, s)
    | NumLit (loc, n) -> NumLit (loc, n)
    | BoolLit (loc, b) -> BoolLit (loc, b)
    | UnitLit loc -> UnitLit loc
    | NullLit loc -> NullLit loc
    | ListLit (loc, exprs) -> ListLit (loc, List.map (rename_expr scope) exprs)
    | MapLit (loc, kvs) -> MapLit (loc, List.map (fun (k, e) -> (k, rename_expr scope e)) kvs)

    | MapLookup (loc, expr, key) -> MapLookup (loc, rename_expr scope expr, key)
    | DynLookup (loc, mexpr, kexpr) -> DynLookup (loc, rename_expr scope mexpr, rename_expr scope kexpr)

    | Add(loc, e1,e2)    -> Add(loc, rename_expr scope e1, rename_expr scope e2)
    | Sub(loc, e1,e2)    -> Sub(loc, rename_expr scope e1, rename_expr scope e2)
    | Mul(loc, e1,e2)    -> Mul(loc, rename_expr scope e1, rename_expr scope e2)
    | Div(loc, e1,e2)    -> Div(loc, rename_expr scope e1, rename_expr scope e2)
    | Concat(loc, e1,e2) -> Concat(loc, rename_expr scope e1, rename_expr scope e2)

    | Equals(loc, e1,e2) -> Equals(loc, rename_expr scope e1, rename_expr scope e2)
    | NotEquals(loc, e1,e2) -> NotEquals(loc, rename_expr scope e1, rename_expr scope e2)
    | LE(loc, e1,e2)     -> LE(loc, rename_expr scope e1, rename_expr scope e2)
    | GE(loc, e1,e2)     -> GE(loc, rename_expr scope e1, rename_expr scope e2)
    | LT(loc, e1,e2)     -> LT(loc, rename_expr scope e1, rename_expr scope e2)
    | GT(loc, e1,e2)     -> GT(loc, rename_expr scope e1, rename_expr scope e2)

    | Or(loc, e1, e2)    -> Or(loc, rename_expr scope e1, rename_expr scope e2)
    | And(loc, e1, e2)   -> And(loc, rename_expr scope e1, rename_expr scope e2)
    | Not(loc, expr)     -> Not(loc, rename_expr scope expr)

    | Range(loc, start_expr, end_expr) -> Range(loc, rename_expr scope start_expr, rename_expr scope end_expr)
    | ListComp(loc, result_expr, comp_exprs) ->
        let rec rename_comp scope renamed_comp_exprs_rev = function
        | [] -> Renamed.ListComp(loc, rename_expr scope result_expr, List.rev renamed_comp_exprs_rev)
        | Parsed.FilterClause expr :: comps ->
            let expr' = rename_expr scope expr in
            rename_comp scope (FilterClause expr' :: renamed_comp_exprs_rev) comps
        | Parsed.DrawClause (pattern, expr) :: comps -> 
            (* The expression is renamed with the previous scope, since
               draw clauses cannot be recursive *)
            let expr' = rename_expr scope expr in
            let pattern', scope_trans = rename_pattern scope pattern in
            rename_comp (scope_trans scope) (DrawClause (pattern', expr') :: renamed_comp_exprs_rev) comps
        in
        rename_comp scope [] comp_exprs

    | If(loc, e1, e2, e3) -> If(loc, rename_expr scope e1, rename_expr scope e2, rename_expr scope e3)

    | Seq (loc, es) -> Seq (loc, rename_seq scope es)

    | LetSeq (loc, _, _) | LetRecSeq (loc, _, _, _) -> raise (RenameError.LetSeqInNonSeq (expr, loc))

    | Let (loc, p, e1, e2) ->
        let p', scope_trans = rename_pattern scope p in
        (* regular lets are non-recursive, so e1 is *not* evaluated in the new scope *)
        Let (loc, p', rename_expr scope e1, rename_expr (scope_trans scope) e2)
    | LetRec (loc, x, patterns, e1, e2) ->
        let x' = fresh_var scope x in
        let patterns', scope_trans = rename_patterns scope patterns in
        let scope' = insert_var x x' scope in
        let inner_scope = scope_trans scope' in
        (* let rec's *are* recursive *)
        LetRec(loc, x', patterns', rename_expr inner_scope e1, rename_expr scope' e2)
    | Assign (loc, x, e) ->
        let x' = lookup_var scope loc x in
        Assign (loc, x', rename_expr scope e)

    | ProgCall (loc, p, args) ->
        ProgCall (loc, p, List.map (rename_expr scope) args)
    | Pipe (loc, exprs) ->
        Pipe (loc, List.map (rename_expr scope) exprs)
    | Async (loc, expr) ->
        Async (loc, rename_expr scope expr)
    | Await (loc, expr) ->
        Await (loc, rename_expr scope expr)
    | Match (loc, expr, branches) ->
        let expr' = rename_expr scope expr in
        let branches' = List.map (
            fun (pat, expr) -> 
                let pat', scope_trans = rename_pattern scope pat in
                let scope' = scope_trans scope in
                (pat', rename_expr scope' expr)
            ) branches 
        in
        Match(loc, expr', branches')

and rename_seq_state (scope : RenameScope.t) (exprs : Parsed.expr list) : Renamed.expr list * RenameScope.t = let open RenameScope in
    match exprs with
    | (LetSeq (loc, p, e) :: exprs) -> 
        let p', scope_trans = rename_pattern scope p in
        (* regular lets are non-recursive, so e' is *not* evaluated in the new scope *)
        let e' = rename_expr scope e in
        let exprs', res_scope = rename_seq_state (scope_trans scope) exprs in
        (LetSeq (loc, p', e') :: exprs', res_scope)
    | LetRecSeq (loc, x, patterns, e) :: exprs -> 
        let x' = fresh_var scope x in
        let patterns', scope_trans = rename_patterns scope patterns in
        let scope' = insert_var x x' scope in
        let inner_scope = scope_trans scope' in
        (* let rec's *are* recursive! *)
        let e' = rename_expr inner_scope e in
        let exprs', res_scope = rename_seq_state scope' exprs in
        (LetRecSeq(loc, x', patterns', e') :: exprs', res_scope)
    | (e :: exprs) -> 
        let e' = rename_expr scope e in
        let exprs', res_state = rename_seq_state scope exprs in
        (e' :: exprs', res_state)
    | [] -> ([], scope)
and rename_seq scope exprs =
    let res, _ = rename_seq_state scope exprs in
    res

let rename_option (scope : RenameScope.t) (flag_def : Parsed.flag_def): Renamed.flag_def * RenameScope.t =
    let args, scope = match flag_def.args with
        | Varargs name -> 
            let name' = RenameScope.fresh_var scope name in
            Renamed.Varargs name', RenameScope.insert_var name name' scope
        | Switch name ->
            let name' = RenameScope.fresh_var scope name in
            Renamed.Switch name', RenameScope.insert_var name name' scope
        | Named args ->
            let args' = List.map (RenameScope.fresh_var scope) args in
            let scope = List.fold_right2 (RenameScope.insert_var) args args' scope in
            Named args', scope
        | NamedDefault args ->
            let args' = List.map (fun (x, def) -> (RenameScope.fresh_var scope x, def)) args in
            let scope = List.fold_right2 (fun (x, _) (y, _) -> RenameScope.insert_var x y) args args' scope in
            NamedDefault args', scope
    in        
    { args
    ; flags = flag_def.flags
    ; description = flag_def.description
    }, scope


let rename_header (scope : RenameScope.t) (header : Parsed.header): Renamed.header * RenameScope.t =
    let rec go scope = function
    | (flag_def::defs) ->
        let flag_def, scope = rename_option scope flag_def in
        let defs, scope = go scope defs in
        flag_def :: defs, scope
    | [] -> [], scope
    in
    let options, scope = go scope header.options in
    { usage = header.usage
    ; description = header.description
    ; options
    }, scope

let rename (header : Parsed.header) (exprs : Parsed.expr list): Renamed.header * Renamed.expr list =
    let headers', env = rename_header RenameScope.empty header in 
    headers', rename_seq env exprs    
