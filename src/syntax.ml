open Classes
open Util

type loc = Loc.t

module StringMap = Map.Make(String)

type name = { name : string; index : Unique.t }

type type_constructor_sort = DataConSort | TypeAliasSort

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

  let equal x y = compare x y = 0

  let fresh (name : string) = { name; index = Unique.fresh () }
  let refresh (name : t) = { name with index = Unique.fresh() }
  
end

module NameMap = Map.Make(Name)

(* Types only diverge between parsed and everything else, so we use a separate functor for these
   to avoid unnecessary coercions *)
module MakeTy(Ext : sig 
    type name
    type mod_subscript_tycon_ext
  end) = struct
  type ty =
    | Forall of Ext.name * ty
    | Fun of ty list * ty
    | TyVar of Ext.name
    | TyConstructor of Ext.name * ty list
    (* Type aliases are kept around as long as possible to improve error messages, but we need to differentiate
       between these and real type constructors *)
    | TypeAlias of Ext.name * ty list
    (* The 'name' is just kept around for error messages but *completely ignored* when typechecking *)
    | Unif of Unique.t * Ext.name
    | Skol of Unique.t * Ext.name
    | Number
    | Bool
    | String
    | Tuple of ty array
    | List of ty
    | Ref of ty
    | Promise of ty
    | RecordClosed of (string * ty) array
    | RecordUnif of (string * ty) array * (Unique.t * Ext.name)
    | RecordSkol of (string * ty) array * (Unique.t * Ext.name)
    | RecordVar of (string * ty) array * Ext.name
    | VariantClosed of (string * ty list) array
    | VariantUnif of (string * ty list) array * (Unique.t * Ext.name)
    | VariantSkol of (string * ty list) array * (Unique.t * Ext.name)
    | VariantVar of (string * ty list) array * Ext.name
    (* We keep subscript type constructors before renaming, but just replace them
       by their (unique!) type constructor in the original module afterwards,
       where mod_subscript_tycon_ext is instantiated to void *)
    | ModSubscriptTyCon of Ext.mod_subscript_tycon_ext * Ext.name * Ext.name * ty list
end

module Template = struct
  include TypeDefinition

  let (-->) xs y = Fun (xs, y)

  module Ty = struct 
    type t = ty
    
    let unit = RecordClosed [||]

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
            | List ty | Promise ty | Ref ty -> go ty
            | RecordClosed fields -> mconcat monoid (Array.to_list (Array.map (go << snd) fields))
            | VariantClosed fields ->
              mconcat monoid (Array.to_list (Array.map (fold monoid go << snd) fields))
            (* We traverse the variable as if it was its own node, so
               callers only need to handle `Unif` to handle all unification variables *)
            | RecordUnif (fields, (u, name)) ->
              M.append (mconcat monoid (Array.to_list (Array.map (go << snd) fields))) (go (Unif (u, name)))
            | VariantUnif (fields, (u, name)) ->
              M.append (mconcat monoid (Array.to_list (Array.map (fold monoid go << snd) fields))) (go (Unif (u, name)))
            (* We do the same for skolems *)
            | RecordSkol (fields, (u, name)) ->
              M.append (fold monoid (go << snd) (Array.to_list fields)) (go (Skol (u, name)))
            | VariantSkol (fields, (u, name)) ->
              M.append (fold monoid (fold monoid go << snd) (Array.to_list fields)) (go (Skol (u, name)))
            | RecordVar (fields, var) -> M.append (mconcat monoid (Array.to_list (Array.map (go << snd) fields))) (go (TyVar var))
            | VariantVar (fields, var) ->
              M.append (mconcat monoid (Array.to_list (Array.map (fold monoid go << snd) fields))) (go (TyVar var))
            | TyConstructor (_name, args) ->
              fold monoid go args
            | TypeAlias (_name, args) ->
              fold monoid go args  
            | ModSubscriptTyCon (_ext, _mod_name, _name, args) ->
              fold monoid go args
            (* non-recursive cases *)
            | TyVar _ | Unif _ | Skol _ | Number | Bool | String -> M.empty
          in
          M.append result remaining
        in
        go ty
      
    (* Like `collect` but returns the result in a list instead of an arbitrary monoid.
       This function uses `Difflist`s internally, so it should be significantly faster
       than `collect monoid_list`*)
    let collect_list : 'a. (t -> 'a list) -> t -> 'a list =
      fun get -> Difflist.to_list << (collect monoid_difflist (Difflist.of_list << get))
  

    let replace_record_extension fields = function
    | Unif (u, name) -> RecordUnif (fields, (u, name))
    | TyVar var -> RecordVar (fields, var)
    | Skol (u, name) -> RecordSkol (fields, (u, name))
    | RecordClosed fields2 ->
      RecordClosed (Array.append fields fields2)
    | RecordUnif (fields2, unif) ->
      RecordUnif (Array.append fields fields2, unif)
    | RecordSkol (fields2, skol) ->
      RecordSkol (Array.append fields fields2, skol)
    | RecordVar (fields2, var) ->
      RecordVar (Array.append fields fields2, var)
    | ty -> panic __LOC__ ("Record extension variable replaced with non-record type")

    let replace_variant_extension fields = function
    | Unif (u, name) -> VariantUnif (fields, (u, name))
    | TyVar var -> VariantVar (fields, var)
    | Skol (u, name) -> VariantSkol (fields, (u, name))
    | VariantClosed fields2 ->
      VariantClosed (Array.append fields fields2)
    | VariantUnif (fields2, unif) ->
      VariantUnif (Array.append fields fields2, unif)
    | VariantSkol (fields2, skol) ->
      VariantSkol (Array.append fields fields2, skol)
    | VariantVar (fields2, var) ->
      VariantVar (Array.append fields fields2, var)
    | ty -> panic __LOC__ ("Variant extension variable replaced with non-variant type")

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
        | Ref ty -> Ref (transform trans ty)
        | RecordClosed tys -> RecordClosed (Array.map (fun (x, ty) -> (x, transform trans ty)) tys)
        | VariantClosed tys -> VariantClosed (Array.map (fun (x, tys) -> (x, List.map (transform trans) tys)) tys)
        | RecordUnif (fields, (u, name)) -> 
          let fields = Array.map (fun (x, ty) -> (x, transform trans ty)) fields in
          let extension_type = transform trans (Unif (u, name)) in
          replace_record_extension fields extension_type
        | VariantUnif (fields, (u, name)) -> 
          let fields = Array.map (fun (x, ty) -> (x, List.map(transform trans) ty)) fields in
          let extension_type = transform trans (Unif (u, name)) in
          replace_variant_extension fields extension_type  
        | RecordVar (fields, var) ->
          let fields = Array.map (fun (x, ty) -> (x, transform trans ty)) fields in
          let extension_type = transform trans (TyVar var) in
          replace_record_extension fields extension_type
        | VariantVar (fields, var) ->
          let fields = Array.map (fun (x, ty) -> (x, List.map (transform trans) ty)) fields in
          let extension_type = transform trans (TyVar var) in
          replace_variant_extension fields extension_type    
        | RecordSkol (fields, (u, name)) ->
          let fields = Array.map (fun (x, ty) -> (x, transform trans ty)) fields in
          let extension_type = transform trans (Skol (u, name)) in
          replace_record_extension fields extension_type
        | VariantSkol (fields, (u, name)) ->
          let fields = Array.map (fun (x, ty) -> (x, List.map (transform trans) ty)) fields in
          let extension_type = transform trans (Skol (u, name)) in
          replace_variant_extension fields extension_type  
        | TyConstructor (name, args) -> TyConstructor (name, List.map (transform trans) args)
        | TypeAlias (name, args) -> TypeAlias (name, List.map (transform trans) args)
        | ModSubscriptTyCon (ext, mod_name, name, args) -> 
          ModSubscriptTyCon (ext, mod_name, name, List.map (transform trans) args)
        (* Non-recursive cases *)
        | (TyVar _ | Unif _ | Skol _ | Number | Bool | String) as ty -> ty
        in
        trans transformed
  end

  type pattern =
    | VarPat of loc * name
    | ConsPat of loc * pattern * pattern
    | ListPat of loc * pattern list
    | TuplePat of loc * pattern list
    | NumPat of loc * float
    | StringPat of loc * string
    | OrPat of loc * pattern * pattern
    | TypePat of loc * pattern * ty
    | DataPat of loc * name * pattern
    | VariantPat of loc * string * pattern list

  type module_exports = {
      exported_variables : name StringMap.t;
      exported_variable_types : ty NameMap.t;

      exported_ty_constructors : (name * int * type_constructor_sort) StringMap.t;
      exported_data_definitions : (name list * ty) NameMap.t;

      exported_type_aliases : (name list * ty) NameMap.t;
    }

  type binop = 
    | Add | Sub | Mul | Div | Concat | Cons | Equals | NotEquals
    | LE | GE | LT | GT | Or | And 

  type module_expr =
    | Import of import_ext * string
    | ModVar of loc * name
    | SubModule of loc * module_expr * name

  and expr =
    (* Lambda calculus *)
    | Var of loc * name                     (* x *)
    | DataConstructor of loc * name         (* X *)
    (* The renamer replaces these by the data constructor with the (unique)
       name from the module where it is defined. This sets mod_subscript_tycon_ext to void *)
    | ModSubscriptDataCon of mod_subscript_tycon_ext * loc * name * name (* M.X *)
    (* Data Constructors vs Variant Constructors are resolved in the renamer.
       `X` is a Variant Constructor iff there is no Data Constructor named `X` in scope. *)
    | VariantConstructor of loc * string * expr list     (* X *)
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
    | ModSubscript of loc * name * name     (* M.x *)
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
    | LetRecSeq of loc * ty option * name * pattern list * expr  (* [let f : ty]; let f(x, .., x) = e*)
    | LetEnvSeq of loc * string * expr            (* let $x = e *)
    | LetDataSeq of loc * name * name list * ty   (* data X(x, .., x) = t *)
    | LetTypeSeq of loc * name * name list * ty   (* type X(x, .., x) = t *)
    (* local definitions *)
    | Let of loc * pattern * expr * expr              (* let p = e1 in e2 (valid everywhere) *)
    | LetRec of loc * ty option * name * pattern list * expr * expr  (* [let f : ty]; let f(x, .., x) = e*)
    | LetEnv of loc * string * expr * expr            (* let $x = e in e *)
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
    (* Types *)
    | Ascription of loc * expr * ty
    | Unwrap of loc * expr  (* e! *)
    (* Mutable references *)
    | Assign of loc * expr * expr                     (* x := e *)
    | MakeRef of loc * expr                           (* ref e *)


  and list_comp_clause =
    | DrawClause of pattern * expr (* p <- e *)
    | FilterClause of expr            (* e *)

  module MExpr = struct
      type t = module_expr
  
      let rec pretty = function
        | Import (_, path) -> "import \"" ^ path ^ "\""
        | ModVar (_, var) -> pretty_name var
        | SubModule (_, mod_expr, name) -> pretty mod_expr ^ "." ^ pretty_name name
  
      let get_loc = function
        | ModVar (loc, _) | SubModule (loc, _, _) -> loc
        | Import (ext, _) -> import_ext_loc ext
  
      let collect : type a. (module Monoid with type t = a) -> (t -> a) -> t -> a =
        fun monoid get mexpr ->
          let (module M) = monoid in
          let rec go mexpr =
            let result = get mexpr in
            let remaining = match mexpr with
              | Import _ | ModVar _ -> M.empty 
              | SubModule (_, mod_expr, _) -> go mod_expr
            in
            M.append result remaining
          in 
          go mexpr
  
      let collect_list : 'a. (t -> 'a list) -> t -> 'a list =
        fun get -> Difflist.to_list << (collect monoid_difflist (Difflist.of_list << get))
      end
  

  module Expr = struct
    let collect : type a. (module Monoid with type t = a) -> (expr -> a) -> expr -> a =
      fun monoid get expr ->
        let (module M) = monoid in
        let rec go expr = 
          let result = get expr in
          let remaining = match expr with
          | Var _ | DataConstructor _ | ModSubscriptDataCon _ | StringLit _ | NumLit _ | BoolLit _ | EnvVar _
          | UnitLit _ | NullLit _ | ModSubscript _ | LetDataSeq _ | LetTypeSeq _ -> M.empty
          | VariantConstructor(_, _, args) ->
            fold monoid go args
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
          | ListComp(_, expr, clauses) -> 
            let go_clause = function
              | DrawClause (_pat, expr) -> go expr
              | FilterClause expr -> go expr
            in
            M.append (go expr) (fold monoid go_clause clauses)
          | If(_, condition, then_expr, else_expr) -> M.append (go condition) (M.append (go then_expr) (go else_expr))
          | Seq(_, exprs) -> fold monoid go exprs
          | LetSeq(_, _, expr) | LetRecSeq(_, _, _, _, expr) | LetEnvSeq(_, _, expr) -> go expr
          | Let(_, _, bind_expr, rest_expr) | LetRec(_, _, _, _, bind_expr, rest_expr) | LetEnv(_, _, bind_expr, rest_expr)
            -> M.append (go bind_expr) (go rest_expr)
          | ProgCall(_, _, arg_exprs) -> fold monoid go arg_exprs
          | Pipe(_, exprs) -> fold monoid go exprs
          | Async (_, expr) -> go expr
          | Await (_, expr) -> go expr
          | Match(_, scrut_expr, branch_exprs) -> M.append (go scrut_expr) (fold monoid (fun (_, expr) -> go expr) branch_exprs)
          | LetModuleSeq _ -> M.empty
          | Ascription (_, expr, _) -> go expr
          | Unwrap(_, expr) -> go expr
          | Assign(_, place_expr, expr) -> M.append (go place_expr) (go expr)
          | MakeRef(_, expr) -> go expr
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

  (** WARNING: SHOULD YOU EVER FALL TO THE TEMPTATION OF MAKING
      export_item DIVERGE BETWEEN Renamed AND Typed, **UPDATE typecheck_exports IN types.ml** 
      IF YOU DON'T, HAVOC WILL ENSUE AS EXPORT ITEMS WILL BE ALIENATED FROM THEIR ORIGIN
      AND ENSLAVED AS DIFFERENT TYPES! 
      DARK MAGIC WILL ENGULF THEIR SOULS AND BREAK YOUR INVARIANTS.
      SEGMENTATION FAULTS WILL ROAM THE LAND! TURN BACK WHILE YOU CAN! *)
  type export_item = ExportVal of loc * name
                   | ExportType of loc * name

  type header = {
      usage: string option
    ; description: string option
    ; options: flag_def list
    ; exports: export_item list
  }

  let rec pretty_type = function
    | Forall (var, ty) -> "∀" ^ pretty_name var ^ ". " ^ pretty_type ty
    | Fun (args, res) -> "(" ^ String.concat ", " (List.map pretty_type args) ^ ") -> " ^ pretty_type res
    (* TODO: Add a flag to disambiguate 0-argument type constructors and type variables *)
    | TyConstructor (name, []) -> pretty_name name
    | TyConstructor (name, args) -> pretty_name name ^ "(" ^ String.concat ", " (List.map pretty_type args) ^ ")"
    | TypeAlias (name, []) -> pretty_name name
    | TypeAlias (name, args) -> pretty_name name ^ "(" ^ String.concat ", " (List.map pretty_type args) ^ ")"
    | ModSubscriptTyCon(_ext, mod_name, name, args) -> 
      pretty_name mod_name ^ "." ^ pretty_name name ^ "(" ^ String.concat ", " (List.map pretty_type args) ^ ")"
    | TyVar var -> pretty_name var
    | Unif (u, name) -> pretty_name name ^ "$" ^ Unique.display u
    | Skol (u, name) -> pretty_name name ^ "@" ^ Unique.display u
    | Number -> "Number"
    | Bool -> "Bool"
    | String -> "String"
    | Tuple tys -> "(" ^ String.concat ", " (Array.to_list (Array.map pretty_type tys)) ^ ")"
    | List ty -> "List(" ^ pretty_type ty ^ ")"
    | Promise ty -> "Promise(" ^ pretty_type ty ^ ")"
    | Ref ty -> "Ref(" ^ pretty_type ty ^ ")"
    (* There is no difference between empty records and unit, so to make typechecking easier,
       this type is represented as an empty record, but for the sake of error messages, it is printed as '()' *)
    | RecordClosed [||] -> "()"
    | RecordClosed fields -> "{ " ^ String.concat ", " (Array.to_list (Array.map (fun (x, ty) -> x ^ " : " ^ pretty_type ty) fields)) ^ " }"
    | RecordUnif (fields, (u, name)) -> "{ " ^ String.concat ", " (Array.to_list (Array.map (fun (x, ty) -> x ^ " : " ^ pretty_type ty) fields)) ^ " | " ^ pretty_type (Unif (u, name)) ^ " }"
    | RecordSkol (fields, (u, name)) -> "{ " ^ String.concat ", " (Array.to_list (Array.map (fun (x, ty) -> x ^ " : " ^ pretty_type ty) fields)) ^ " | " ^ pretty_type (Skol (u, name)) ^ " }"
    | RecordVar (fields, name) -> "{ " ^ String.concat ", " (Array.to_list (Array.map (fun (x, ty) -> x ^ " : " ^ pretty_type ty) fields)) ^ " | " ^ pretty_name name ^ " }"
    | VariantClosed fields -> "< " ^ String.concat ", " (Array.to_list (Array.map (fun (x, tys) -> x ^ "(" ^ String.concat ", " (List.map pretty_type tys) ^ ")") fields)) ^ " >"
    | VariantUnif (fields, (u, name)) -> "< " ^ String.concat ", " (Array.to_list (Array.map (fun (x, tys) -> x ^ "(" ^ String.concat ", " (List.map pretty_type tys) ^ ")") fields)) ^ " | " ^ pretty_type (Unif (u, name)) ^ " >"
    | VariantSkol (fields, (u, name)) -> "< " ^ String.concat ", " (Array.to_list (Array.map (fun (x, tys) -> x ^ "(" ^ String.concat ", " (List.map pretty_type tys) ^ ")") fields)) ^ " | " ^ pretty_type (Skol (u, name)) ^ " >"
    | VariantVar (fields, name) -> "< " ^ String.concat ", " (Array.to_list (Array.map (fun (x, tys) -> x ^ "(" ^ String.concat ", " (List.map pretty_type tys) ^ ")") fields)) ^ " | " ^ pretty_name name ^ " >"

  let rec pretty_pattern = function
    | VarPat (_, x) -> pretty_name x
    | ConsPat (_, x, xs) -> "(" ^ pretty_pattern x ^ ") :: (" ^ pretty_pattern xs ^ ")"
    | ListPat (_, pats) -> "[" ^ String.concat ", " (List.map pretty_pattern pats) ^ "]"
    | TuplePat (_, pats) -> "(" ^ String.concat ", " (List.map pretty_pattern pats) ^ ")"
    | NumPat (_, f) -> Float.to_string f
    | StringPat (_, literal) -> "\"" ^ literal ^ "\""
    | OrPat (_, p1, p2) -> "(" ^ pretty_pattern p1 ^ " | " ^ pretty_pattern p2 ^ ")"
    | TypePat (_, pat, ty) -> "(" ^ pretty_pattern pat ^ " : " ^ pretty_type ty ^ ")"
    | DataPat (_, name, pattern) -> pretty_name name ^ "(" ^ pretty_pattern pattern ^ ")"
    | VariantPat (_, name, patterns) -> "`" ^ name ^ "(" ^ String.concat ", " (List.map pretty_pattern patterns) ^ ")"

  let rec pretty = function
    | Var (_, x) -> pretty_name x
    | DataConstructor(_, x) -> pretty_name x
    | VariantConstructor(_, x, args) -> "`" ^ x ^ "(" ^ String.concat ", " (List.map pretty args) ^ ")"
    | ModSubscriptDataCon(_, _, mod_name, name) -> pretty_name mod_name ^ "." ^ pretty_name name
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
    | RecordLit (_, kvs) -> "{" ^ String.concat ", " (List.map (fun (k, e) -> k ^ " = " ^ pretty e) kvs) ^ "}"
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
    | BinOp (_, e1, Cons, e2)    -> "(" ^ pretty e1 ^ " :: " ^ pretty e2 ^ ")"

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
    | LetRecSeq (_, None, x, xs, e) -> "let " ^ pretty_name x ^ "(" ^ String.concat ", " (List.map pretty_pattern xs) ^ ") = " ^ pretty e
    | LetRecSeq (_, Some ty, x, xs, e) -> "let " ^ pretty_name x ^ " : " ^ pretty_type ty ^ "; let " ^ pretty_name x ^ "(" ^ String.concat ", " (List.map pretty_pattern xs) ^ ") = " ^ pretty e
    | LetEnvSeq (_, x, e) -> "let $" ^ x ^ " = " ^ pretty e
    | LetDataSeq (_, varname, [], ty) -> "data " ^ pretty_name varname ^ " = " ^ pretty_type ty
    | LetDataSeq (_, varname, params, ty) -> "data " ^ pretty_name varname ^ "(" ^ String.concat ", " (List.map pretty_name params) ^ ") = " ^ pretty_type ty
    | LetTypeSeq (_, varname, [], ty) -> "type " ^ pretty_name varname ^ " = " ^ pretty_type ty
    | LetTypeSeq (_, varname, params, ty) -> "type " ^ pretty_name varname ^ "(" ^ String.concat ", " (List.map pretty_name params) ^ ") = " ^ pretty_type ty


    | Let (_, x, e1, e2) ->
        "let " ^ pretty_pattern x ^ " = " ^ pretty e1 ^ " in " ^ pretty e2
    | LetRec (_, None, x, xs, e1, e2) -> "let rec " ^ pretty_name x ^ "(" ^ String.concat ", " (List.map pretty_pattern xs) ^ ") = " ^ pretty e1 ^ " in " ^ pretty e2
    | LetRec (_, Some ty, x, xs, e1, e2) -> "let " ^ pretty_name x ^ " : " ^ pretty_type ty ^ "; let " ^ pretty_name x ^ "(" ^ String.concat ", " (List.map pretty_pattern xs) ^ ") = " ^ pretty e1 ^ " in " ^ pretty e2
    | LetEnv (_, x, e1, e2) -> "let $" ^ x ^ " = " ^ pretty e1 ^ " in " ^ pretty e2
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
    | Ascription (_, expr, ty) ->
      "(" ^ pretty expr ^ " : " ^ pretty_type ty ^ ")"
    | Unwrap(_, expr) -> pretty expr ^ "!"
    | MakeRef(_, expr) -> "ref " ^ pretty expr ^ ""
    | Assign (_, x, e) -> pretty x ^ " = " ^ pretty e

  let pretty_list (exprs : expr list) : string =
    List.fold_right (fun x r -> pretty x ^ "\n" ^ r) exprs ""

  let get_loc = function
    | Var (loc, _) | DataConstructor(loc, _) | VariantConstructor(loc, _, _) | ModSubscriptDataCon(_, loc, _, _) | App (loc, _, _) | Lambda (loc, _, _) | StringLit (loc, _) | NumLit (loc, _)
    | BoolLit (loc, _) | UnitLit loc | NullLit loc | ListLit(loc, _) | TupleLit(loc, _) | RecordLit(loc, _) 
    | Subscript(loc, _, _) | RecordUpdate (loc, _, _) | RecordExtension (loc, _, _) | DynLookup(loc, _, _) 
    | BinOp(loc, _, _, _) | Not(loc, _)
    | Range(loc, _, _) | ListComp(loc, _, _)
    | If(loc, _, _, _) | Seq(loc, _) | LetSeq(loc, _, _) | LetRecSeq(loc, _, _, _, _) | LetEnvSeq(loc, _, _) 
    | LetDataSeq (loc, _, _, _) | LetTypeSeq(loc, _, _, _) | Let(loc, _, _, _)
    | LetRec(loc, _, _, _, _, _) | LetEnv(loc, _, _, _) | Assign(loc, _, _) | ProgCall(loc, _, _) | Pipe(loc, _) | EnvVar(loc, _)
    | Async(loc, _) | Await(loc, _) | Match(loc, _, _) | LetModuleSeq(loc, _, _) | Ascription (loc, _, _) | Unwrap(loc, _) | ModSubscript (loc, _, _)
    | MakeRef(loc, _)
    -> loc

  let get_pattern_loc = function
    | VarPat (loc, _) | ConsPat(loc, _, _) | ListPat (loc, _) | TuplePat (loc, _)
    | NumPat (loc, _) | StringPat(loc, _) | OrPat (loc, _, _) | TypePat (loc, _, _) | DataPat (loc, _, _)
    | VariantPat(loc, _, _)
    -> loc


  module Traversal = struct
    (* The traversal system here is similar to the one used in ppx_lib for the OCaml AST.
       Though unlike that one, there are not separate traversal classes for map, fold, etc here.
       Instead, every traversal performs a combined map/fold operation.

       The syntax tree is always traversed in a bottom-up manner.
      *)

    class ['state] traversal = object(self)
      method expr : 'state -> expr -> (expr * 'state) =
        fun state expr -> (expr, state)

      method pattern : 'state -> pattern -> (pattern * 'state) =
        fun state pattern -> (pattern, state)

      method ty : 'state -> ty -> (ty * 'state) =
        fun state ty -> (ty, state)

      method name : 'state -> name -> (name * 'state) =
        fun state name -> (name, state)

      method module_expr : 'state -> module_expr -> (module_expr * 'state) = 
        fun state module_expr -> (module_expr, state)

      method list_comp_clause : 'state -> list_comp_clause -> (list_comp_clause * 'state) = 
        fun state list_comp_clause -> (list_comp_clause, state)

      method traverse_expr : 'state -> expr -> (expr * 'state) = 
        fun state expression ->
          let transformed, state = match expression with
          (* Non-recursive cases (that don't mention another traversable node type) *)
          | StringLit _ | NumLit _ | BoolLit _ | UnitLit _ | EnvVar _
          | NullLit _ -> expression, state
          (* Recursive boilerplate *)
          | Var (loc, name) ->
            let name, state = self#traverse_name state name in
            Var (loc, name), state
          | DataConstructor (loc, name) ->
            let name, state = self#traverse_name state name in
            DataConstructor (loc, name), state
          | VariantConstructor (loc, name, args) ->
            let args, state = self#traverse_list self#traverse_expr state args in
            VariantConstructor (loc, name, args), state
          | ModSubscriptDataCon (ext, loc, mod_name, name) ->
            let mod_name, state = self#traverse_name state mod_name in
            let name, state = self#traverse_name state name in
            ModSubscriptDataCon (ext, loc, mod_name, name), state
          | App (loc, fun_expr, arg_exprs) ->
            let fun_expr, state = self#traverse_expr state fun_expr in
            let (arg_exprs, state) = self#traverse_list self#traverse_expr state arg_exprs in
            App (loc, fun_expr, arg_exprs), state
          | Lambda(loc, patterns, body) ->
            let patterns, state = self#traverse_list self#traverse_pattern state patterns in
            let body, state = self#traverse_expr state body in
            Lambda(loc, patterns, body), state
          | ListLit(loc, exprs) ->
            let exprs, state = self#traverse_list self#traverse_expr state exprs in
            ListLit(loc, exprs), state
          | TupleLit(loc, exprs) ->
            let exprs, state = self#traverse_list self#traverse_expr state exprs in
            TupleLit(loc, exprs), state
          | RecordLit (loc, fields) ->
            let fields, state = self#traverse_list 
              (fun state (field, expr) ->
                let expr, state = self#traverse_expr state expr in
                (field, expr), state) state fields 
            in
            RecordLit(loc, fields), state
          | Subscript(loc, expr, field) ->
            let expr, state = self#traverse_expr state expr in
            Subscript(loc, expr, field), state
          | RecordUpdate(loc, expr, updates) ->
            let expr, state = self#traverse_expr state expr in
            let updates, state = self#traverse_list 
              (fun state (field, expr) ->
                let expr, state = self#traverse_expr state expr in
                (field, expr), state) state updates 
            in
            RecordUpdate(loc, expr, updates), state
          | RecordExtension(loc, expr, updates) ->
            let expr, state = self#traverse_expr state expr in
            let updates, state = self#traverse_list 
              (fun state (field, expr) ->
                let expr, state = self#traverse_expr state expr in
                (field, expr), state) state updates 
            in
            RecordExtension(loc, expr, updates), state
          | DynLookup(loc, expr, field_expr) ->
            let expr, state = self#traverse_expr state expr in
            let field_expr, state = self#traverse_expr state field_expr in
            DynLookup(loc, expr, field_expr), state
          | BinOp(loc, left_expr, op, right_expr) -> 
            let left_expr, state = self#traverse_expr state left_expr in
            let right_expr, state = self#traverse_expr state right_expr in
            BinOp(loc, left_expr, op, right_expr), state
          | Not(loc, expr) ->
            let expr, state = self#traverse_expr state expr in
            Not(loc, expr), state
          | Range(loc, start_expr, end_expr) ->
            let start_expr, state = self#traverse_expr state start_expr in
            let end_expr, state = self#traverse_expr state end_expr in
            Range(loc, start_expr, end_expr), state
          | ListComp(loc, expr, clauses) -> 
            let expr, state = self#traverse_expr state expr in
            let clauses, state = self#traverse_list self#traverse_list_comp_clause state clauses in
            ListComp(loc, expr, clauses), state
          | If(loc, condition_expr, then_expr, else_expr) ->
            let condition_expr, state = self#traverse_expr state condition_expr in
            let then_expr, state = self#traverse_expr state then_expr in
            let else_expr, state = self#traverse_expr state else_expr in
            If(loc, condition_expr, then_expr, else_expr), state
          | Seq(loc, exprs) ->
            let exprs, state = self#traverse_list self#traverse_expr state exprs in
            Seq(loc, exprs), state
          | LetSeq(loc, pattern, expr) ->
            let pattern, state = self#traverse_pattern state pattern in
            let expr, state = self#traverse_expr state expr in
            LetSeq(loc, pattern, expr), state
          | LetRecSeq(loc, maybe_typesig, fun_name, param_patterns, body_expr) ->
            let maybe_typesig, state = match maybe_typesig with
            | None -> None, state
            | Some ty -> 
              let ty, state = self#traverse_type state ty in
              Some ty, state
            in
            let fun_name, state = self#traverse_name state fun_name in
            let param_patterns, state = self#traverse_list self#traverse_pattern state param_patterns in
            let body_expr, state = self#traverse_expr state body_expr in
            LetRecSeq(loc, maybe_typesig, fun_name, param_patterns, body_expr), state
          | LetEnvSeq(loc, env_var, expr) ->
            let expr, state = self#traverse_expr state expr in
            LetEnvSeq(loc, env_var, expr), state
          | LetDataSeq(loc, data_name, params, ty) ->
            let data_name, state = self#traverse_name state data_name in
            let params, state = self#traverse_list self#traverse_name state params in
            let ty, state = self#traverse_type state ty in
            LetDataSeq(loc, data_name, params, ty), state
          | LetTypeSeq(loc, data_name, params, ty) ->
            let data_name, state = self#traverse_name state data_name in
            let params, state = self#traverse_list self#traverse_name state params in
            let ty, state = self#traverse_type state ty in
            LetTypeSeq(loc, data_name, params, ty), state  
          | Let(loc, pattern, expr, rest_expr) ->
            let pattern, state = self#traverse_pattern state pattern in
            let expr, state = self#traverse_expr state expr in
            let rest_expr, state = self#traverse_expr state rest_expr in
            Let(loc, pattern, expr, rest_expr), state
          | LetRec(loc, maybe_typesig, fun_name, param_patterns, body_expr, rest_expr) ->
            let maybe_typesig, state = match maybe_typesig with
            | None -> None, state
            | Some ty -> 
              let ty, state = self#traverse_type state ty in
              Some ty, state
            in
            let fun_name, state = self#traverse_name state fun_name in
            let param_patterns, state = self#traverse_list self#traverse_pattern state param_patterns in
            let body_expr, state = self#traverse_expr state body_expr in
            let rest_expr, state = self#traverse_expr state rest_expr in
            LetRec(loc, maybe_typesig, fun_name, param_patterns, body_expr, rest_expr), state
          | LetEnv(loc, envvar, expr, rest_expr) ->
            let expr, state = self#traverse_expr state expr in
            let rest_expr, state = self#traverse_expr state rest_expr in
            LetEnv(loc, envvar, expr, rest_expr), state
          | Assign(loc, place_expr, expr) ->
            let place_expr, state = self#traverse_expr state place_expr in
            let expr, state = self#traverse_expr state expr in
            Assign(loc, place_expr, expr), state
          | ProgCall(loc, progname, exprs) ->
            let exprs, state = self#traverse_list self#traverse_expr state exprs in
            ProgCall(loc, progname, exprs), state
          | Pipe(loc, exprs) ->
            let exprs, state = self#traverse_list self#traverse_expr state exprs in
            Pipe(loc, exprs), state
          | Async(loc, expr) -> 
            let expr, state = self#traverse_expr state expr in
            Async(loc, expr), state
          | Await(loc, expr) -> 
            let expr, state = self#traverse_expr state expr in
            Await(loc, expr), state
          | Match (loc, scrutinee_expr, branch_exprs) ->
            let scrutinee_expr, state = self#traverse_expr state scrutinee_expr in
            let branch_exprs, state = self#traverse_list 
              (fun state (pattern, expr) ->
                let pattern, state = self#traverse_pattern state pattern in
                let expr, state = self#traverse_expr state expr in
                (pattern, expr), state) state branch_exprs
            in
            Match(loc, scrutinee_expr, branch_exprs), state
          | LetModuleSeq(loc, name, mod_expr) ->
            let name, state = self#traverse_name state name in
            let mod_expr, state = self#traverse_module_expr state mod_expr in
            LetModuleSeq(loc, name, mod_expr), state 
          | Ascription(loc, expr, ty) ->
            let expr, state = self#traverse_expr state expr in
            let ty, state = self#traverse_type state ty in
            Ascription(loc, expr, ty), state
          | ModSubscript(ext, mod_name, field_name) ->
            let mod_name, state = self#traverse_name state mod_name in
            let field_name, state = self#traverse_name state field_name in
            ModSubscript(ext, mod_name, field_name), state
          | Unwrap(loc, expr) ->
            let expr, state = self#traverse_expr state expr in
            Unwrap(loc, expr), state
          | MakeRef(loc, expr) ->
            let expr, state = self#traverse_expr state expr in
            MakeRef(loc, expr), state
          in
          self#expr state transformed

    method traverse_pattern : 'state -> pattern -> (pattern * 'state) =
      fun state pattern ->
        let transformed, state = match pattern with
        (* Non-recursive *)
        | NumPat(loc, num) -> NumPat(loc, num), state
        | StringPat(loc, num) -> StringPat(loc, num), state
        (* Recursive *)
        | VarPat(loc, name) -> 
          let name, state = self#traverse_name state name in
          VarPat(loc, name), state
        | ConsPat(loc, head_pattern, tail_pattern) -> 
          let head_pattern, state = self#traverse_pattern state head_pattern in
          let tail_pattern, state = self#traverse_pattern state tail_pattern in
          ConsPat(loc, head_pattern, tail_pattern), state
        | ListPat(loc, patterns) ->
          let patterns, state = self#traverse_list self#traverse_pattern state patterns in
          ListPat(loc, patterns), state
        | TuplePat(loc, patterns) ->
          let patterns, state = self#traverse_list self#traverse_pattern state patterns in
          TuplePat(loc, patterns), state
        | OrPat(loc, left_pattern, right_pattern) ->
          let left_pattern, state = self#traverse_pattern state left_pattern in
          let right_pattern, state = self#traverse_pattern state right_pattern in
          OrPat(loc, left_pattern, right_pattern), state
        | TypePat(loc, pattern, ty) ->
          let pattern, state = self#traverse_pattern state pattern in
          let ty, state = self#traverse_type state ty in
          TypePat(loc, pattern, ty), state
        | DataPat(loc, constructor_name, pattern) ->
          let constructor_name, state = self#traverse_name state constructor_name in
          let patterns, state = self#traverse_pattern state pattern in
          DataPat(loc, constructor_name, patterns), state
        | VariantPat(loc, unqualified_name, patterns) ->
          let patterns, state = self#traverse_list self#traverse_pattern state patterns in
          VariantPat(loc, unqualified_name, patterns), state
        in
        self#pattern state transformed

    method traverse_type : 'state -> ty -> (ty * 'state) =
      fun state ty ->
        let transformed, state = match ty with
        (* Non-recursive cases *)
        | Number | Bool | String -> ty, state
        (* Recursion *)
        | Forall (var_name, ty) ->
          let var_name, state = self#traverse_name state var_name in
          let ty, state = self#traverse_type state ty in
          Forall(var_name, ty), state
        | Fun (dom_types, cod_type) ->
          let dom_types, state = self#traverse_list self#traverse_type state dom_types in
          let cod_type, state = self#traverse_type state cod_type in
          Fun(dom_types, cod_type), state
        | TyVar(name) ->
          let name, state = self#traverse_name state name in
          TyVar(name), state
        | TyConstructor(name, arg_types) ->
          let name, state = self#traverse_name state name in
          let arg_types, state = self#traverse_list self#traverse_type state arg_types in
          TyConstructor(name, arg_types), state
        | TypeAlias(name, arg_types) ->
          let name, state = self#traverse_name state name in
          let arg_types, state = self#traverse_list self#traverse_type state arg_types in
          TypeAlias(name, arg_types), state  
        | ModSubscriptTyCon(ext, mod_name, name, arg_types) ->
          let mod_name, state = self#traverse_name state mod_name in
          let name, state = self#traverse_name state name in
          let arg_types, state = self#traverse_list self#traverse_type state arg_types in
          ModSubscriptTyCon(ext, mod_name, name, arg_types), state
        | Unif(unique, name) ->
          let name, state = self#traverse_name state name in
          Unif(unique, name), state
        | Skol(unique, name) ->
          let name, state = self#traverse_name state name in
          Skol(unique, name), state
        | Tuple(tys) ->
          let tys, state = self#traverse_list self#traverse_type state (Array.to_list tys) in
          Tuple(Array.of_list tys), state
        | List(ty) ->
          let ty, state = self#traverse_type state ty in
          List(ty), state
        | Promise(ty) ->
          let ty, state = self#traverse_type state ty in
          List(ty), state
        | Ref(ty) ->
          let ty, state = self#traverse_type state ty in
          Ref(ty), state
        | RecordClosed fields ->
          let fields, state = self#traverse_list 
            (fun state (field, ty) ->
              let ty, state = self#traverse_type state ty in
              (field, ty), state
              ) 
            state 
            (Array.to_list fields) 
          in
          RecordClosed (Array.of_list fields), state
        | VariantClosed fields ->
          let fields, state = self#traverse_list 
            (fun state (field, tys) ->
              let tys, state = self#traverse_list self#traverse_type state tys in
              (field, tys), state
              ) 
            state 
            (Array.to_list fields) 
          in
          VariantClosed (Array.of_list fields), state  
        | RecordVar (fields, var) ->
          let fields, state = self#traverse_list 
            (fun state (field, ty) ->
              let ty, state = self#traverse_type state ty in
              (field, ty), state
              ) 
            state 
            (Array.to_list fields) 
          in
          (* Just as in Ty.transform, we treat record extension variables as freestanding
            types and properly merge the transformed results back to give a single record type
            TODO: Maybe this should be configurable? *)
          let var, state = self#traverse_type state (TyVar var) in
          Ty.replace_record_extension (Array.of_list fields) var, state
        | VariantVar (fields, var) ->
          let fields, state = self#traverse_list 
            (fun state (field, tys) ->
              let tys, state = self#traverse_list self#traverse_type state tys in
              (field, tys), state
              ) 
            state 
            (Array.to_list fields) 
          in
          let var, state = self#traverse_type state (TyVar var) in
          Ty.replace_variant_extension (Array.of_list fields) var, state  
        | RecordUnif (fields, (unique, var)) ->
          let fields, state = self#traverse_list 
            (fun state (field, ty) ->
              let ty, state = self#traverse_type state ty in
              (field, ty), state
              ) 
            state 
            (Array.to_list fields) 
          in
          let var, state = self#traverse_type state (Unif (unique, var)) in
          Ty.replace_record_extension (Array.of_list fields) var, state
        | VariantUnif (fields, (unique, var)) ->
          let fields, state = self#traverse_list 
            (fun state (field, tys) ->
              let tys, state = self#traverse_list self#traverse_type state tys in
              (field, tys), state
              ) 
            state 
            (Array.to_list fields) 
          in
          let var, state = self#traverse_type state (Unif (unique, var)) in
          Ty.replace_variant_extension (Array.of_list fields) var, state  
        | RecordSkol (fields, (unique, var)) ->
          let fields, state = self#traverse_list 
            (fun state (field, ty) ->
              let ty, state = self#traverse_type state ty in
              (field, ty), state
              ) 
            state 
            (Array.to_list fields) 
          in
          let var, state = self#traverse_type state (Skol (unique, var)) in
          Ty.replace_record_extension (Array.of_list fields) var, state
        | VariantSkol (fields, (unique, var)) ->
          let fields, state = self#traverse_list 
            (fun state (field, tys) ->
              let tys, state = self#traverse_list self#traverse_type state tys in
              (field, tys), state
              ) 
            state 
            (Array.to_list fields) 
          in
          let var, state = self#traverse_type state (Skol (unique, var)) in
          Ty.replace_variant_extension (Array.of_list fields) var, state    
        in
        self#ty state transformed

    method traverse_name : 'state -> name -> (name * 'state) =
      fun state name -> self#name state name

    method traverse_module_expr : 'state -> module_expr -> (module_expr * 'state) =
      fun state module_expr ->
        let transformed, state = match module_expr with
        | Import (loc, path) ->
          Import (loc, path), state
        | ModVar (loc, varname) ->
          let varname, state = self#traverse_name state varname in
          ModVar (loc, varname), state
        | SubModule(loc, mod_expr, name) ->
          let mod_expr, state = self#traverse_module_expr state mod_expr in
          let name, state = self#traverse_name state name in
          SubModule(loc, mod_expr, name), state
        in
        self#module_expr state transformed  

    method traverse_list_comp_clause : 'state -> list_comp_clause -> (list_comp_clause * 'state) =
      fun state list_comp_clause ->
        let transformed, state = match list_comp_clause with
        | DrawClause (pattern, expr) ->
          let pattern, state = self#traverse_pattern state pattern in
          let expr, state = self#traverse_expr state expr in
          DrawClause (pattern, expr), state
        | FilterClause (expr) ->
          let expr, state = self#traverse_expr state expr in
          FilterClause(expr), state
        in
        self#list_comp_clause state transformed      

    method traverse_list 
      : 'state 'node. ('state -> 'node -> ('node * 'state)) -> 'state -> 'node list -> ('node list * 'state) 
      = fun traversal state exprs ->
        List.fold_right
          (fun expr (exprs, state) -> 
            let expr, state = traversal state expr in
            (expr :: exprs, state)
            )
          exprs
          ([], state)
    end
  end
end [@@ttg_template]


module ParsedTypeDefinition = MakeTy(struct
  type name = string
  type mod_subscript_tycon_ext = unit
end)

module FullTypeDefinition = MakeTy(struct
  type name = Name.t
  type mod_subscript_tycon_ext = void
end)

module Parsed = Template (struct
  module TypeDefinition = ParsedTypeDefinition

  type name = string

  let pretty_name x = x

  type import_ext = loc
  let import_ext_loc x = x

  type mod_subscript_tycon_ext = unit
end) [@@ttg_pass]


module Typed = Template (struct
  module TypeDefinition = FullTypeDefinition

  type name = Name.t
    
  let pretty_name = Name.pretty

  type import_ext = loc * module_exports * expr list
  let import_ext_loc (loc, _, _) = loc

  type mod_subscript_tycon_ext = void
end) [@@ttg_pass]

type module_exports = Typed.module_exports

(* Typed and Renamed need to be defined in reverse order since
   Renamed depends on Typed.module_exports *)
module Renamed = Template (struct
  module TypeDefinition = FullTypeDefinition

  type name = Name.t
  
  let pretty_name = Name.pretty

  type import_ext = loc * Typed.module_exports * Typed.expr list
  let import_ext_loc (loc, _, _) = loc

  type mod_subscript_tycon_ext = void
end) [@@ttg_pass]


let coerce_bin_op : Renamed.binop -> Typed.binop =
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
