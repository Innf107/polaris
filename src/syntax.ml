open Classes
open Util

type loc = Athena.loc
module Loc = Athena.Loc

module Make (Exts : sig
  type name

  type mod_subscript_ext
  val mod_subscript_loc : mod_subscript_ext -> loc

  val pretty_name : name -> string
end) =
struct
  type name = Exts.name
  let pretty_name = Exts.pretty_name

  type ty =
    | Forall of name * ty
    | Fun of ty list * ty
    | Var of name
    (* The 'name' is just kept around for error messages but *completely ignored* when typechecking *)
    | Unif of Unique.t * name
    | Number
    | Bool
    | String
    | Tuple of ty array
    | List of ty
    | Promise of ty
    | Record of row
    (* TODO: type classes *)
  and row =
    | RowClosed of (string * ty) array
    | RowUnif of (string * ty) array * (Unique.t * name)
    | RowVar of (string * ty) array * name

  let (-->) xs y = Fun (xs, y)

  module Ty = struct 
    type t = ty

    (** Recursively apply a list returning operation over every constructor of 
      a type and concatenate the results with a provided monoid implementation.
      The type is traversed in pre-order
      The results are collected in a `Difflist.t`, so concatenation is very efficient *)
    let collect : type a. (module Monoid with type t = a) -> (t -> a) -> t -> a =
      fun monoid get ty ->
        let (module M) = monoid in
        let rec go ty = 
          let result = get ty in
          let remaining = match ty with
            | Forall (_, ty) -> go ty
            | Fun (args, ty) ->
              M.append (mconcat monoid (List.map go args)) (go ty)
            | Tuple tys ->
              mconcat monoid (Array.to_list (Array.map go tys))
            | List ty | Promise ty -> go ty
            | Record (RowClosed fields) ->
              mconcat monoid (Array.to_list (Array.map (go << snd) fields))
            (* We traverse the variable as if it was its own node, so
               callers only need to handle `Unif` to handle all unification variables *)
            | Record (RowUnif (fields, (u, name))) ->
              M.append (mconcat monoid (Array.to_list (Array.map (go << snd) fields))) (go (Unif (u, name)))
            | Record (RowVar (fields, var)) ->
              M.append (mconcat monoid (Array.to_list (Array.map (go << snd) fields))) (go (Var var))
            (* non-recursive cases *)
            | Var _ | Unif _ | Number | Bool | String  -> M.empty
          in
          M.append result remaining
        in
        go ty
      
    (* Like `collect` but returns the result in a list instead of an arbitrary monoid.
       This function uses `Difflist`s internally, so it should be significantly faster
       than `collect monoid_list`*)
    let collect_list : 'a. (t -> 'a list) -> t -> 'a list =
      fun get -> Difflist.to_list << (collect monoid_difflist (Difflist.of_list << get))
  
    (* Recursively apply a transformation function over every node of a type 
       (in a bottom-up fashion / post-order traversal). *)
    let rec transform : (t -> t) -> t -> t =
      fun trans ty ->
        let transformed = match ty with
        | Forall (x, ty) -> Forall (x, transform trans ty)
        | Fun (args, res) -> Fun (List.map (transform trans) args, transform trans res)
        | Tuple tys -> Tuple (Array.map (transform trans) tys)
        | List ty -> List (transform trans ty)
        | Promise ty -> Promise (transform trans ty)
        | Record (RowClosed tys) -> Record (RowClosed (Array.map (fun (x, ty) -> (x, transform trans ty)) tys))
        | Record (RowUnif (fields, (u, name))) -> 
          let fields = Array.map (fun (x, ty) -> (x, transform trans ty)) fields in
          begin match transform trans (Unif (u, name)) with
          | Unif (u, name) -> Record (RowUnif (fields, (u, name)))
          | Var var -> Record (RowVar (fields, var))
          | Record (RowClosed fields2) ->
            Record (RowClosed (Array.append fields fields2))
          | Record (RowUnif (fields2, unif)) ->
            Record (RowUnif (Array.append fields fields2, unif))
          | Record (RowVar (fields2, var)) ->
            Record (RowVar (Array.append fields fields2, var))
          | ty -> panic __LOC__ ("Row extension variable replaced with non-row type")
          end
        | Record (RowVar (fields, var)) ->
          let fields = Array.map (fun (x, ty) -> (x, transform trans ty)) fields in
          begin match transform trans (Var var) with
          | Unif (u, name) -> Record (RowUnif (fields, (u, name)))
          | Var var -> Record (RowVar (fields, var))
          | Record (RowClosed fields2) ->
            Record (RowClosed (Array.append fields fields2))
          | Record (RowUnif (fields2, unif)) ->
            Record (RowUnif (Array.append fields fields2, unif))
          | Record (RowVar (fields2, var)) ->
            Record (RowVar (Array.append fields fields2, var))
          | ty -> panic __LOC__ ("Row extension variable replaced with non-row type")
          end
        (* Non-recursive cases *)
        | (Var _ | Unif _ | Number | Bool | String) as ty -> ty
        in
        trans transformed
  end

  type pattern =
    | VarPat of loc * name
    | ConsPat of loc * pattern * pattern
    | ListPat of loc * pattern list
    | TuplePat of loc * pattern list
    | NumPat of loc * float
    | OrPat of loc * pattern * pattern

  type module_expr =
    | Import of loc * string
    | ModVar of loc * name

  module MExpr = struct
    type t = module_expr

    let pretty = function
      | Import (_, path) -> "import \"" ^ path ^ "\""
      | ModVar (_, var) -> Exts.pretty_name var

    let get_loc = function
      | Import (loc, _) | ModVar (loc, _) -> loc

    let collect : type a. (module Monoid with type t = a) -> (t -> a) -> t -> a =
      fun monoid get mexpr ->
        let (module M) = monoid in
        let rec go mexpr =
          let result = get mexpr in
          let remaining = match mexpr with
            | Import _ | ModVar _ -> M.empty in
          M.append result remaining
        in 
        go mexpr

    let collect_list : 'a. (t -> 'a list) -> t -> 'a list =
      fun get -> Difflist.to_list << (collect monoid_difflist (Difflist.of_list << get))
    end

  type binop = Add | Sub | Mul | Div | Concat | Equals | NotEquals
             | LE | GE | LT | GT | Or | And

  type expr =
    (* Lambda calculus *)
    | Var of loc * name                     (* x *)
    | App of loc * expr * expr list         (* e (e₁, .., eₙ) *)
    | Lambda of loc * pattern list * expr   (* \(p₁, .., pₙ) -> e*)
    (* Literals *)
    | StringLit of loc * string             (* "str" *)
    | NumLit of loc * float                 (* f *)
    | BoolLit of loc * bool                 (* true | false*)
    | UnitLit of loc                        (* () *)
    | NullLit of loc                        (* null *)
    | ListLit of loc * expr list            (* [e, .., e] *)
    | TupleLit of loc * expr list           (* (e, .., e) *)
    (* Records *)
    | RecordLit of loc * (string * expr) list  (* #{x₁: e₁, .., xₙ: eₙ}*)
    | Subscript of loc * expr * string      (* e.x *)
    | ModSubscript of Exts.mod_subscript_ext * name * name     (* M.x *)
    | RecordUpdate of loc * expr * (string * expr) list (* #{r with x₁: e₁, .., xₙ: eₙ} *)
    | RecordExtension of loc * expr * (string * expr) list (* #{r extend x₁: e₁, .., xₙ: eₙ} *)

    | DynLookup of loc * expr * expr        (* e[e] *)
    (* Common Operations *)
    | BinOp of loc * expr * binop * expr    (* e ° e*)
    | Not of loc * expr                     (* not e *)

    | Range of loc * expr * expr            (* [e .. e] *)
    | ListComp of loc * expr * list_comp_clause list (* [e | c, .., c] *)

    (* Branching *)
    | If of loc * expr * expr * expr        (* if e then e else e*)
    (* Sequencing *)
    | Seq of loc * expr list                      (* { e₁ ; .. ; eₙ } *)
    | LetSeq of loc * pattern * expr              (* let p = e (Only valid inside `Seq` expressions) *)
    | LetRecSeq of loc * name * pattern list * expr  (* let rec f(x, .., x) = e*)
    | LetEnvSeq of loc * string * expr            (* let $x = e *)
    (* Mutable local definitions *)
    | Let of loc * pattern * expr * expr              (* let p = e1 in e2 (valid everywhere) *)
    | LetRec of loc * name * pattern list * expr * expr  (* let rec f(x, .., x) = e*)
    | LetEnv of loc * string * expr * expr            (* let $x = e in e *)
    | Assign of loc * name * expr                     (* x := e *)
    (* Scripting capabilities *)
    | ProgCall of loc * string * expr list  (* !p e₁ .. eₙ *)
    | Pipe of loc * expr list               (* (e₁ | .. | eₙ) *)
    | EnvVar of loc * string                (* $var *)
    (* Async / Await (colorless) *)
    | Async of loc * expr                   (* async e *)
    | Await of loc * expr                   (* await e*)
    (* Pattern matching *)
    | Match of loc * expr * (pattern * expr) list
    (* Modules *)
    | LetModuleSeq of loc * name * module_expr

  and list_comp_clause =
    | DrawClause of pattern * expr (* p <- e *)
    | FilterClause of expr            (* e *)

  module Expr = struct
    let collect : type a. (module Monoid with type t = a) -> (expr -> a) -> expr -> a =
      fun monoid get expr ->
        let (module M) = monoid in
        let rec go expr = 
          let result = get expr in
          let remaining = match expr with
          | Var _ | StringLit _ | NumLit _ | BoolLit _ | EnvVar _
          | UnitLit _ | NullLit _ | ModSubscript _ -> M.empty
          | App(_, fexpr, arg_exprs) -> M.append (go fexpr) (fold monoid go arg_exprs) 
          | Lambda(_, _, expr) -> go expr
          | ListLit(_, exprs) -> fold monoid go exprs
          | TupleLit(_, exprs) -> fold monoid go exprs
          | RecordLit(_, fields) -> fold monoid (fun (_, expr) -> go expr) fields
          | Subscript(_, expr, field) -> go expr
          | RecordUpdate(_, rec_expr, updates) -> M.append (go rec_expr) (fold monoid (fun (_, expr) -> go expr) updates)
          | RecordExtension(_, rec_expr, exts) -> M.append (go rec_expr) (fold monoid (fun (_, expr) -> go expr) exts)
          | DynLookup(_, expr1, expr2) -> M.append (go expr1) (go expr2)
          | BinOp(_, expr1, _, expr2) 
            -> M.append (go expr1) (go expr2)
          | Not(_, expr) -> go expr
          | Range(_, expr1, expr2) -> M.append (go expr1) (go expr2)
          | ListComp(_, expr, clauses) -> todo __LOC__
          | If(_, condition, then_expr, else_expr) -> M.append (go condition) (M.append (go then_expr) (go else_expr))
          | Seq(_, exprs) -> fold monoid go exprs
          | LetSeq(_, _, expr) | LetRecSeq(_, _, _, expr) | LetEnvSeq(_, _, expr) -> go expr
          | Let(_, _, bind_expr, rest_expr) | LetRec(_, _, _, bind_expr, rest_expr) | LetEnv(_, _, bind_expr, rest_expr)
            -> M.append (go bind_expr) (go rest_expr)
          | Assign(_, _, expr) -> go expr
          | ProgCall(_, _, arg_exprs) -> fold monoid go arg_exprs
          | Pipe(_, exprs) -> fold monoid go exprs
          | Async (_, expr) -> go expr
          | Await (_, expr) -> go expr
          | Match(_, scrut_expr, branch_exprs) -> M.append (go scrut_expr) (fold monoid (fun (_, expr) -> go expr) branch_exprs)
          | LetModuleSeq _ -> M.empty
          in
          M.append result remaining
        in
        go expr

    let collect_list = 
      fun get -> Difflist.to_list << (collect monoid_difflist (Difflist.of_list << get))

  end

  type flag_args =
    | Varargs of name
    | Switch of name
    | Named of name list
    | NamedDefault of (name * string) list 

  type flag_def = {
    flags: string list
  ; args: flag_args
  ; description: string option
  }

  type export_item = ExportVal of loc * name

  type header = {
      usage: string option
    ; description: string option
    ; options: flag_def list
    ; exports: export_item list
  }

  let rec pretty_type = function
    | Forall (var, ty) -> "∀" ^ pretty_name var ^ ". " ^ pretty_type ty
    | Fun (args, res) -> "(" ^ String.concat ", " (List.map pretty_type args) ^ ") -> " ^ pretty_type res
    | Var var -> pretty_name var
    | Unif (u, name) -> pretty_name name ^ "$" ^ Unique.display u
    | Number -> "Number"
    | Bool -> "Bool"
    | String -> "String"
    | Tuple tys -> "(" ^ String.concat ", " (Array.to_list (Array.map pretty_type tys)) ^ ")"
    | List ty -> "List(" ^ pretty_type ty ^ ")"
    | Promise ty -> "Promise(" ^ pretty_type ty ^ ")"
    | Record (RowClosed fields) -> "#{" ^ String.concat ", " (Array.to_list (Array.map (fun (x, ty) -> x ^ " : " ^ pretty_type ty) fields)) ^ "}"
    | Record (RowUnif (fields, (u, name))) -> "#{" ^ String.concat ", " (Array.to_list (Array.map (fun (x, ty) -> x ^ " : " ^ pretty_type ty) fields)) ^ " | " ^ pretty_type (Unif (u, name)) ^ "}"
    | Record (RowVar (fields, name)) -> "#{" ^ String.concat ", " (Array.to_list (Array.map (fun (x, ty) -> x ^ " : " ^ pretty_type ty) fields)) ^ " | " ^ pretty_name name ^ "}"

  let rec pretty_pattern = function
    | VarPat (_, x) -> pretty_name x
    | ConsPat (_, x, xs) -> "(" ^ pretty_pattern x ^ ") : (" ^ pretty_pattern xs ^ ")"
    | ListPat (_, pats) -> "[" ^ String.concat ", " (List.map pretty_pattern pats) ^ "]"
    | TuplePat (_, pats) -> "(" ^ String.concat ", " (List.map pretty_pattern pats) ^ ")"
    | NumPat (_, f) -> Float.to_string f
    | OrPat (_, p1, p2) -> "(" ^ pretty_pattern p1 ^ " | " ^ pretty_pattern p2 ^ ")"

  let rec pretty = function
    | Var (_, x) -> pretty_name x
    | App (_, f, args) ->
        pretty f ^ "(" ^ String.concat ", " (List.map pretty args) ^ ")"
    | Lambda (_, params, e) ->
        "\\("
        ^ String.concat ", " (List.map pretty_pattern params)
        ^ ") -> " ^ pretty e
    | StringLit (_,l) -> "\"" ^ l ^ "\""
    | NumLit (_, f) -> string_of_float f
    | BoolLit (_, b) -> string_of_bool b
    | UnitLit _ -> "()"
    | NullLit _ -> "null"
    | ListLit (_, exprs) -> "[" ^ String.concat ", " (List.map pretty exprs) ^ "]"
    | TupleLit (_, exprs) -> "(" ^ String.concat ", " (List.map pretty exprs) ^ ")"
    | RecordLit (_, kvs) -> "#{" ^ String.concat ", " (List.map (fun (k, e) -> k ^ " = " ^ pretty e) kvs) ^ "}"
    | Subscript (_, expr, key) -> "(" ^ pretty expr ^ ")." ^ key
    | ModSubscript (_, mod_name, key_name) -> pretty_name mod_name ^ "." ^ pretty_name key_name
    | RecordUpdate (_, expr, kvs) -> "#{" ^ pretty expr ^ " with " ^ String.concat ", " (List.map (fun (k, e) -> k ^ " = " ^ pretty e) kvs) ^ "}"
    | RecordExtension (_, expr, kvs) -> "#{" ^ pretty expr ^ " extend " ^ String.concat ", " (List.map (fun (k, e) -> k ^ " = " ^ pretty e) kvs) ^ "}"

    | DynLookup (_, mexpr, kexpr) -> "(" ^ pretty mexpr ^ ")[" ^ pretty kexpr ^ "]"

    | BinOp (_, e1, Add, e2)     -> "(" ^ pretty e1 ^ " + " ^ pretty e2 ^ ")"
    | BinOp (_, e1, Sub, e2)     -> "(" ^ pretty e1 ^ " - " ^ pretty e2 ^ ")"
    | BinOp (_, e1, Mul, e2)     -> "(" ^ pretty e1 ^ " * " ^ pretty e2 ^ ")"
    | BinOp (_, e1, Div, e2)     -> "(" ^ pretty e1 ^ " / " ^ pretty e2 ^ ")"
    | BinOp (_, e1, Concat, e2)  -> "(" ^ pretty e1 ^ " .. " ^ pretty e2 ^ ")"

    | BinOp (_, e1, Equals, e2) -> "(" ^ pretty e1 ^ " == " ^ pretty e2 ^ ")"
    | BinOp (_, e1, NotEquals, e2) -> "(" ^ pretty e1 ^ " != " ^ pretty e2 ^ ")"
    | BinOp (_, e1, LE, e2)     -> "(" ^ pretty e1 ^ " <= " ^ pretty e2 ^ ")"
    | BinOp (_, e1, GE, e2)     -> "(" ^ pretty e1 ^ " >= " ^ pretty e2 ^ ")"
    | BinOp (_, e1, LT, e2)     -> "(" ^ pretty e1 ^ " <  " ^ pretty e2 ^ ")"
    | BinOp (_, e1, GT, e2)     -> "(" ^ pretty e1 ^ " >  " ^ pretty e2 ^ ")"

    | BinOp (_, e1, Or, e2)     -> "(" ^ pretty e1 ^ " || " ^ pretty e2 ^ ")"
    | BinOp (_, e1, And, e2)    -> "(" ^ pretty e1 ^ " && " ^ pretty e2 ^ ")"
    | Not (_, e)         -> "(not " ^ pretty e ^ ")"

    | Range(_, e1, e2) -> "[" ^ pretty e1 ^ " .. " ^ pretty e2 ^ "]"
    | ListComp(_, e, clauses) -> 
      let pretty_list_comp = function
      | DrawClause (x, e) -> pretty_pattern x ^ " <- " ^ pretty e
      | FilterClause e -> pretty e
      in 
      "[" ^ pretty e ^ " | " ^ String.concat ", " (List.map pretty_list_comp clauses)

    | If (_, e1, e2, e3) -> "if " ^ pretty e1 ^ " then " ^ pretty e2 ^ " else " ^ pretty e3

    | Seq (_, exprs) -> "{ " ^ String.concat "; " (List.map pretty exprs) ^ "}"
    | LetSeq (_, x, e) -> "let " ^ pretty_pattern x ^ " = " ^ pretty e
    | LetRecSeq (_, x, xs, e) -> "let rec " ^ pretty_name x ^ "(" ^ String.concat ", " (List.map pretty_pattern xs) ^ ") = " ^ pretty e
    | LetEnvSeq (_, x, e) -> "let $" ^ x ^ " = " ^ pretty e
    | Let (_, x, e1, e2) ->
        "let " ^ pretty_pattern x ^ " = " ^ pretty e1 ^ " in " ^ pretty e2
    | LetRec (_, x, xs, e1, e2) -> "let rec " ^ pretty_name x ^ "(" ^ String.concat ", " (List.map pretty_pattern xs) ^ ") = " ^ pretty e1 ^ " in " ^ pretty e2
    | LetEnv (_, x, e1, e2) -> "let $" ^ x ^ " = " ^ pretty e1 ^ " in " ^ pretty e2
    | Assign (_, x, e) -> pretty_name x ^ " = " ^ pretty e
    | ProgCall (_, prog, args) ->
        "!" ^ prog ^ " " ^ String.concat " " (List.map pretty args)
    | Pipe (_, exprs) -> String.concat " | " (List.map pretty exprs)
    | EnvVar (_, x) -> "$" ^ x
    | Async (_, expr) -> "async " ^ pretty expr
    | Await (_, expr) -> "await " ^ pretty expr

    | Match(_, expr, pats) -> 
      "match " ^ pretty expr ^ " {"
      ^ "\n    " ^ String.concat ("\n    ") (List.map (fun (p, e) -> pretty_pattern p ^ " -> " ^ pretty e) pats)
      ^ "\n}"
    | LetModuleSeq (_, name, mexpr) ->
      "module " ^ pretty_name name ^ " = " ^ MExpr.pretty mexpr

  let pretty_list (exprs : expr list) : string =
    List.fold_right (fun x r -> pretty x ^ "\n" ^ r) exprs ""

  let get_loc = function
    | Var (loc, _) | App (loc, _, _) | Lambda (loc, _, _) | StringLit (loc, _) | NumLit (loc, _)
    | BoolLit (loc, _) | UnitLit loc | NullLit loc | ListLit(loc, _) | TupleLit(loc, _) | RecordLit(loc, _) 
    | Subscript(loc, _, _) | RecordUpdate (loc, _, _) | RecordExtension (loc, _, _) | DynLookup(loc, _, _) 
    | BinOp(loc, _, _, _) | Not(loc, _)
    | Range(loc, _, _) | ListComp(loc, _, _)
    | If(loc, _, _, _) | Seq(loc, _) | LetSeq(loc, _, _) | LetRecSeq(loc, _, _, _) | LetEnvSeq(loc, _, _) | Let(loc, _, _, _)
    | LetRec(loc, _, _, _, _) | LetEnv(loc, _, _, _) | Assign(loc, _, _) | ProgCall(loc, _, _) | Pipe(loc, _) | EnvVar(loc, _)
    | Async(loc, _) | Await(loc, _) | Match(loc, _, _) | LetModuleSeq(loc, _, _)
    -> loc
    | ModSubscript (ext, _, _) -> Exts.mod_subscript_loc ext

  let get_pattern_loc = function
    | VarPat (loc, _) | ConsPat(loc, _, _) | ListPat (loc, _) | TuplePat (loc, _)
    | NumPat (loc, _) | OrPat (loc, _, _) 
    -> loc
end

type name = { name : string; index : Unique.t }

module Name = struct
  type t = name

  let original_name (name : t) = name.name

  let pretty (name : t) = name.name ^ "_" ^ Unique.display name.index

  let primop_index = Unique.fresh ()

  (* Comparisons are purely based on the name index
     and therefore have no actual meaning.
     We have to make sure to compare primops by their name though!
  *)
  let compare (x : t) (y : t) : int = 
    if x.index = primop_index && y.index = primop_index then
      String.compare x.name y.name 
    else
      Unique.compare x.index y.index

  let fresh (name : string) = { name; index = Unique.fresh () }
  let refresh (name : t) = { name with index = Unique.fresh() }
  
end

module StringMap = Map.Make(String)
module NameMap = Map.Make(Name)

module Parsed = Make (struct
  type name = string
  type mod_subscript_ext = void
  let mod_subscript_loc = absurd

  let pretty_name (x : name) = x
end)

module Renamed = Make (struct
  type name = Name.t

  type mod_subscript_ext = loc
  let mod_subscript_loc loc = loc
  
  let pretty_name = Name.pretty
end)

type module_exports = {
  exported_names : name StringMap.t;
  exported_types : Renamed.ty NameMap.t;
}
