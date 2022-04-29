open Ast
open Util

module RenameMap = Map.Make(String)

module RenameError = struct
    exception VarNotFound of string
    exception LetSeqInNonSeq of StringExpr.expr
end

module RenameScope = struct
    open RenameMap
    type t = {counter: int ref; variables: name RenameMap.t}

    (* TODO: Handle shadowing sensibly somehow *)
    let insert_var (old : string) (renamed : name) (scope : t) : t =
        { scope with variables = add old renamed scope.variables }
    
    (* TODO: How do exceptions work in OCaml? *)
    let lookup_var (scope : t) (var : string) : 'name =
        try 
            find var scope.variables 
        with
            Not_found -> raise (RenameError.VarNotFound var)

    let fresh_var (scope : t) (var : string) : 'name =
        let ix = !(scope.counter) in
        scope.counter := ix + 1;
        { name = var; index = ix }
end


let rec rename_expr (scope : RenameScope.t) (expr : string_expr): name_expr = let open RenameScope in
    match expr with 
    | Var var_name -> Var (lookup_var scope var_name)
    | App (f, args) -> App (rename_expr scope f, List.map (rename_expr scope) args)
    
    | Lambda (xs, e) ->
        let xs' = List.map (fresh_var scope) xs in
        let scope' = List.fold_right2 insert_var xs xs' scope in
        Lambda (xs', rename_expr scope' e)

    | StringLit s -> StringLit s
    | NumLit n -> NumLit n
    | UnitLit -> UnitLit

    | Add(e1,e2) -> Add(rename_expr scope e1, rename_expr scope e2)
    | Sub(e1,e2) -> Sub(rename_expr scope e1, rename_expr scope e2)
    | Mul(e1,e2) -> Mul(rename_expr scope e1, rename_expr scope e2)
    | Div(e1,e2) -> Div(rename_expr scope e1, rename_expr scope e2)

    | Equals(e1,e2) -> Equals(rename_expr scope e1, rename_expr scope e2)
    | LE(e1,e2)     -> LE(rename_expr scope e1, rename_expr scope e2)
    | GE(e1,e2)     -> GE(rename_expr scope e1, rename_expr scope e2)
    | LT(e1,e2)     -> LT(rename_expr scope e1, rename_expr scope e2)
    | GT(e1,e2)     -> GT(rename_expr scope e1, rename_expr scope e2)

    | If(e1, e2, e3) -> If(rename_expr scope e1, rename_expr scope e2, rename_expr scope e3)

    | Seq es -> Seq (rename_seq scope es)

    | LetSeq _ | LetRecSeq _ -> raise (RenameError.LetSeqInNonSeq expr)

    | Let (x, e1, e2) -> 
        let x' = fresh_var scope x in
        let scope' = insert_var x x' scope in
        (* regular lets are non-recursive! *)
        Let (x', rename_expr scope e1, rename_expr scope' e2)
    | LetRec (x, params, e1, e2) ->
        let x' = fresh_var scope x in
        let params' = List.map (fresh_var scope) params in
        let scope' = insert_var x x' scope in
        (* let rec's *are* recursive *)
        LetRec(x', params', rename_expr scope' e1, rename_expr scope e2)
    | Assign (x, e) ->
        let x' = lookup_var scope x in
        Assign (x', rename_expr scope e)

    | Print e -> Print (rename_expr scope e)

    | ProgCall (p, args) ->
        ProgCall (p, List.map (rename_expr scope) args)
    | Pipe exprs ->
        Pipe (List.map (rename_expr scope) exprs)

and rename_seq (scope : RenameScope.t) (exprs : string_expr list) : name_expr list = let open RenameScope in
    match exprs with
    | (LetSeq (x, e) :: exprs) -> 
        let x' = fresh_var scope x in
        let scope' = insert_var x x' scope in
        (* regular lets are non-recursive! *)
        LetSeq (x', rename_expr scope e) :: rename_seq scope' exprs
    | LetRecSeq (x, params, e) :: exprs -> 
        let x' = fresh_var scope x in
        let scope' = insert_var x x' scope in
        let params' = List.map (fresh_var scope) params in
        (* let rec's *are* recursive! *)
        LetRecSeq(x', params', rename_expr scope' e) :: rename_seq scope' exprs
    | (e :: exprs) -> rename_expr scope e :: rename_seq scope exprs
    | [] -> []

let rename (exprs : string_expr list): name_expr list =
    rename_seq {counter = ref 0; variables = RenameMap.empty} exprs    
