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
    | IntLit n -> IntLit n
    | FloatLit n -> FloatLit n
    | UnitLit -> UnitLit

    | Seq es -> Seq (rename_seq scope es)

    | LetSeq (x, e) -> raise (RenameError.LetSeqInNonSeq expr)
    
    | Let (x, e1, e2) -> 
        let x' = fresh_var scope x in
        let scope' = insert_var x x' scope in
        (* lets are non-recursive! *)
        Let (x', rename_expr scope e1, rename_expr scope' e2)
    
    | Assign (x, e) ->
        let x' = lookup_var scope x in
        Assign (x', rename_expr scope e)

    | ProgCall (p, args) ->
        ProgCall (p, List.map (rename_expr scope) args)
    | Pipe exprs ->
        Pipe (List.map (rename_expr scope) exprs)

and rename_seq (scope : RenameScope.t) (exprs : string_expr list) : name_expr list = let open RenameScope in
    match exprs with
    | (LetSeq (x, e) :: exprs) -> 
        let x' = fresh_var scope x in
        let scope' = insert_var x x' scope in
        (* lets are non-recursive! *)
        LetSeq (x', rename_expr scope e) :: rename_seq scope' exprs
    | (e :: exprs) -> rename_expr scope e :: rename_seq scope exprs
    | [] -> []

let rename (exprs : string_expr list): name_expr list =
    rename_seq {counter = ref 0; variables = RenameMap.empty} exprs    
