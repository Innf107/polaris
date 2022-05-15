open Ast
open Util

module RenameMap = Map.Make(String)

module RenameError = struct
    exception VarNotFound of string * loc
    exception LetSeqInNonSeq of StringExpr.expr * loc
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


let rec rename_expr (scope : RenameScope.t) (expr : string_expr): name_expr = let open RenameScope in
    match expr with 
    | Var (loc, var_name) ->
        if Primops.is_primop var_name
        then Var(loc, {name=var_name; index= -1})
        else Var (loc, lookup_var scope loc var_name)
    | App (loc, f, args) -> App (loc, rename_expr scope f, List.map (rename_expr scope) args)
    
    | Lambda (loc, xs, e) ->
        let xs' = List.map (fresh_var scope) xs in
        let scope' = List.fold_right2 insert_var xs xs' scope in
        Lambda (loc, xs', rename_expr scope' e)

    | StringLit (loc, s) -> StringLit (loc, s)
    | NumLit (loc, n) -> NumLit (loc, n)
    | BoolLit (loc, b) -> BoolLit (loc, b)
    | UnitLit loc -> UnitLit loc
    | NullLit loc -> NullLit loc
    | ListLit (loc, exprs) -> ListLit (loc, List.map (rename_expr scope) exprs)

    | Add(loc, e1,e2)    -> Add(loc, rename_expr scope e1, rename_expr scope e2)
    | Sub(loc, e1,e2)    -> Sub(loc, rename_expr scope e1, rename_expr scope e2)
    | Mul(loc, e1,e2)    -> Mul(loc, rename_expr scope e1, rename_expr scope e2)
    | Div(loc, e1,e2)    -> Div(loc, rename_expr scope e1, rename_expr scope e2)
    | Concat(loc, e1,e2) -> Concat(loc, rename_expr scope e1, rename_expr scope e2)

    | Equals(loc, e1,e2) -> Equals(loc, rename_expr scope e1, rename_expr scope e2)
    | LE(loc, e1,e2)     -> LE(loc, rename_expr scope e1, rename_expr scope e2)
    | GE(loc, e1,e2)     -> GE(loc, rename_expr scope e1, rename_expr scope e2)
    | LT(loc, e1,e2)     -> LT(loc, rename_expr scope e1, rename_expr scope e2)
    | GT(loc, e1,e2)     -> GT(loc, rename_expr scope e1, rename_expr scope e2)

    | If(loc, e1, e2, e3) -> If(loc, rename_expr scope e1, rename_expr scope e2, rename_expr scope e3)

    | Seq (loc, es) -> Seq (loc, rename_seq scope es)

    | LetSeq (loc, _, _) | LetRecSeq (loc, _, _, _) -> raise (RenameError.LetSeqInNonSeq (expr, loc))

    | Let (loc, x, e1, e2) -> 
        let x' = fresh_var scope x in
        let scope' = insert_var x x' scope in
        (* regular lets are non-recursive! *)
        Let (loc, x', rename_expr scope e1, rename_expr scope' e2)
    | LetRec (loc, x, params, e1, e2) ->
        let x' = fresh_var scope x in
        let params' = List.map (fresh_var scope) params in
        let scope' = insert_var x x' scope in
        let inner_scope = List.fold_right2 insert_var params params' scope' in
        (* let rec's *are* recursive *)
        LetRec(loc, x', params', rename_expr inner_scope e1, rename_expr scope' e2)
    | Assign (loc, x, e) ->
        let x' = lookup_var scope loc x in
        Assign (loc, x', rename_expr scope e)

    | Print (loc, e) -> Print (loc, rename_expr scope e)

    | ProgCall (loc, p, args) ->
        ProgCall (loc, p, List.map (rename_expr scope) args)
    | Pipe (loc, exprs) ->
        Pipe (loc, List.map (rename_expr scope) exprs)

and rename_seq_state (scope : RenameScope.t) (exprs : string_expr list) : name_expr list * RenameScope.t = let open RenameScope in
    match exprs with
    | (LetSeq (loc, x, e) :: exprs) -> 
        let x' = fresh_var scope x in
        let scope' = insert_var x x' scope in
        (* regular lets are non-recursive! *)
        let e' = rename_expr scope e in
        let exprs', res_scope = rename_seq_state scope' exprs in
        (LetSeq (loc, x', e') :: exprs', res_scope)
    | LetRecSeq (loc, x, params, e) :: exprs -> 
        let x' = fresh_var scope x in
        let params' = List.map (fresh_var scope) params in
        let scope' = insert_var x x' scope in
        let inner_scope = List.fold_right2 insert_var params params' scope' in
        (* let rec's *are* recursive! *)
        let e' = rename_expr inner_scope e in
        let exprs', res_scope = rename_seq_state scope' exprs in
        (LetRecSeq(loc, x', params', e') :: exprs', res_scope)
    | (e :: exprs) -> 
        let e' = rename_expr scope e in
        let exprs', res_state = rename_seq_state scope exprs in
        (e' :: exprs', res_state)
    | [] -> ([], scope)
and rename_seq scope exprs =
    let res, _ = rename_seq_state scope exprs in
    res

let rename (exprs : string_expr list): name_expr list =
    rename_seq RenameScope.empty exprs    
