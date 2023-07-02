open Classes
open Util

type loc = Loc.t

module StringMap = Map.Make (String)

type name = {
  name : string;
  index : Unique.t;
}

type type_constructor_sort =
  | DataConSort
  | TypeAliasSort

type data_constructor_sort =
  | NewtypeConSort
  | ExceptionSort

module Name = struct
  type t = name

  let original_name (name : t) = name.name

  let pretty (name : t) =
    if Config.verbose_names () then name.name ^ "_" ^ Unique.display name.index
    else name.name

  let primop_index = Unique.fresh ()

  (* Comparisons are purely based on the name index
     and therefore have no actual meaning.
     We have to make sure to compare primops by their name though!
  *)
  let compare (x : t) (y : t) : int =
    if x.index = primop_index && y.index = primop_index then
      String.compare x.name y.name
    else Unique.compare x.index y.index

  let equal x y = compare x y = 0
  let fresh (name : string) = { name; index = Unique.fresh () }
  let refresh (name : t) = { name with index = Unique.fresh () }
end

module NameMap = Map.Make (Name)

(* Types only diverge between parsed and everything else, so we use a separate functor for these
   to avoid unnecessary coercions *)
module MakeTy (Ext : sig
  type name
  type mod_subscript_tycon_ext
end) =
struct
  type ty =
    | Forall of Ext.name * ty
    | Fun of ty list * ty
    | TyVar of Ext.name
    | TyConstructor of Ext.name * ty list
    (* Type aliases are kept around as long as possible to improve error messages, but we need to differentiate
       between these and real type constructors *)
    | TypeAlias of Ext.name * ty list
    (* The 'name' is just kept around for error messages but *completely ignored* when typechecking *)
    | Unif of ty Typeref.t * Ext.name
    | Skol of Unique.t * Typeref.level * Ext.name
    | Number
    | Bool
    | String
    | Exception
    | Tuple of ty array
    | List of ty
    | Ref of ty
    | Promise of ty
    | RecordClosed of (string * ty) array
    | RecordUnif of (string * ty) array * (ty Typeref.t * Ext.name)
    | RecordSkol of (string * ty) array * (Unique.t * Typeref.level * Ext.name)
    | RecordVar of (string * ty) array * Ext.name
    | VariantClosed of (string * ty list) array
    | VariantUnif of (string * ty list) array * (ty Typeref.t * Ext.name)
    | VariantSkol of
        (string * ty list) array * (Unique.t * Typeref.level * Ext.name)
    | VariantVar of (string * ty list) array * Ext.name
    (* We keep subscript type constructors before renaming, but just replace them
       by their (unique!) type constructor in the original module afterwards,
       where mod_subscript_tycon_ext is instantiated to void *)
    | ModSubscriptTyCon of
        Ext.mod_subscript_tycon_ext * Ext.name * Ext.name * ty list
end

module Template = struct
  include TypeDefinition

  let ( --> ) xs y = Fun (xs, y)

  module Ty = struct
    type t = ty

    let unit = RecordClosed [||]

    let replace_record_extension fields = function
      | Unif (typeref, name) -> RecordUnif (fields, (typeref, name))
      | TyVar var -> RecordVar (fields, var)
      | Skol (u, level, name) -> RecordSkol (fields, (u, level, name))
      | RecordClosed fields2 -> RecordClosed (Array.append fields fields2)
      | RecordUnif (fields2, unif) ->
          RecordUnif (Array.append fields fields2, unif)
      | RecordSkol (fields2, skol) ->
          RecordSkol (Array.append fields fields2, skol)
      | RecordVar (fields2, var) -> RecordVar (Array.append fields fields2, var)
      | ty ->
          panic __LOC__
            "Record extension variable replaced with non-record type"

    let replace_variant_extension fields = function
      | Unif (typeref, name) -> VariantUnif (fields, (typeref, name))
      | TyVar var -> VariantVar (fields, var)
      | Skol (u, _level, name) -> VariantSkol (fields, (u, _level, name))
      | VariantClosed fields2 -> VariantClosed (Array.append fields fields2)
      | VariantUnif (fields2, unif) ->
          VariantUnif (Array.append fields fields2, unif)
      | VariantSkol (fields2, skol) ->
          VariantSkol (Array.append fields fields2, skol)
      | VariantVar (fields2, var) ->
          VariantVar (Array.append fields fields2, var)
      | ty ->
          panic __LOC__
            "Variant extension variable replaced with non-variant type"

    let rec normalize_unif : ty -> ty = function
      | Unif (typeref, name) as ty -> begin
          match Typeref.get typeref with
          | Unbound _ -> ty
          | Bound inner_ty -> normalize_unif inner_ty
        end
      | RecordUnif (fields, (typeref, name)) as ty -> begin
          match Typeref.get typeref with
          | Unbound _ -> ty
          | Bound inner_ty ->
              replace_record_extension fields (normalize_unif inner_ty)
        end
      | VariantUnif (fields, (typeref, name)) as ty -> begin
          match Typeref.get typeref with
          | Unbound _ -> ty
          | Bound inner_ty ->
              replace_variant_extension fields (normalize_unif inner_ty)
        end
      | ty -> ty
  end

  type pattern =
    | VarPat of VarPatExt.t * name
    | AsPat of loc * pattern * name
    | ConsPat of loc * pattern * pattern
    | ListPat of loc * pattern list
    | TuplePat of loc * pattern list
    | NumPat of loc * float
    | StringPat of loc * string
    | BoolPat of loc * bool
    | OrPat of loc * pattern * pattern
    | TypePat of loc * pattern * ty
    | DataPat of loc * name * pattern
    | ExceptionDataPat of loc * name * pattern list
    | VariantPat of VariantPatExt.t * string * pattern list

  type module_exports = {
    exported_variables : name StringMap.t;
    exported_variable_types : ty NameMap.t;
    exported_ty_constructors : (name * int * type_constructor_sort) StringMap.t;
    exported_data_definitions : (name list * ty) NameMap.t;
    exported_exceptions : ty list NameMap.t;
    exported_type_aliases : (name list * ty) NameMap.t;
  }

  type binop =
    | Add
    | Sub
    | Mul
    | Div
    | Concat
    | Cons
    | Equals
    | NotEquals
    | LE
    | GE
    | LT
    | GT
    | Or
    | And

  type module_expr =
    | Import of import_ext * string
    | ModVar of loc * name
    | SubModule of loc * module_expr * name

  and expr =
    (* Lambda calculus *)
    | Var of VarExt.t * name (* x *)
    | DataConstructor of DataConExt.t * name (* X *)
    | ExceptionConstructor of loc * name
    (* The renamer replaces these by the data constructor with the (unique)
       name from the module where it is defined. This sets mod_subscript_tycon_ext to void *)
    | ModSubscriptDataCon of
        mod_subscript_tycon_ext * loc * name * name (* M.X *)
    (* Data Constructors vs Variant Constructors are resolved in the renamer.
       `X` is a Variant Constructor iff there is no Data Constructor named `X` in scope. *)
    | VariantConstructor of loc * string * expr list (* X *)
    | App of loc * expr * expr list (* e (e₁, .., eₙ) *)
    | Lambda of loc * pattern list * expr (* \(p₁, .., pₙ) -> e*)
    (* Literals *)
    | StringLit of loc * string (* "str" *)
    | NumLit of loc * float (* f *)
    | BoolLit of loc * bool (* true | false*)
    | UnitLit of loc (* () *)
    | ListLit of loc * expr list (* [e, .., e] *)
    | TupleLit of loc * expr list (* (e, .., e) *)
    | StringInterpolation of loc * interpolation_component list
    (* Records *)
    | RecordLit of loc * (string * expr) list (* #{x₁: e₁, .., xₙ: eₙ}*)
    | Subscript of SubscriptExt.t * expr * string (* e.x *)
    | ModSubscript of ModSubscriptExt.t * name * name (* M.x *)
    | RecordUpdate of
        loc * expr * (string * expr) list (* #{r with x₁: e₁, .., xₙ: eₙ} *)
    | RecordExtension of
        loc * expr * (string * expr) list (* #{r extend x₁: e₁, .., xₙ: eₙ} *)
    | DynLookup of loc * expr * expr (* e[e] *)
    (* Common Operations *)
    | BinOp of loc * expr * binop * expr (* e ° e*)
    | Not of loc * expr (* not e *)
    | Range of loc * expr * expr (* [e .. e] *)
    | ListComp of loc * expr * list_comp_clause list (* [e | c, .., c] *)
    (* Branching *)
    | If of loc * expr * expr * expr (* if e then e else e*)
    (* Sequencing *)
    | Seq of loc * expr list (* { e₁ ; .. ; eₙ } *)
    | LetSeq of
        loc
        * pattern
        * expr (* let p = e (Only valid inside `Seq` expressions) *)
    | LetRecSeq of
        LetRecExt.t
        * ty option
        * name
        * pattern list
        * expr (* [let f : ty]; let f(x, .., x) = e*)
    | LetEnvSeq of loc * string * expr (* let $x = e *)
    | LetDataSeq of loc * name * name list * ty (* data X(x, .., x) = t *)
    | LetTypeSeq of loc * name * name list * ty (* type X(x, .., x) = t *)
    (* Scripting capabilities *)
    | ProgCall of loc * string * expr list (* !p e₁ .. eₙ *)
    | Pipe of loc * expr list (* (e₁ | .. | eₙ) *)
    | EnvVar of loc * string (* $var *)
    (* Async / Await (colorless) *)
    | Async of loc * expr (* async e *)
    | Await of loc * expr (* await e*)
    (* Pattern matching *)
    | Match of loc * expr * (pattern * expr) list
    (* Modules *)
    | LetModuleSeq of loc * name * module_expr
    (* Types *)
    | Ascription of loc * expr * ty
    | Unwrap of loc * expr (* e! *)
    (* Mutable references *)
    | Assign of loc * expr * expr (* x := e *)
    | MakeRef of loc * expr (* ref e *)
    (* Exceptions *)
    | LetExceptionSeq of
        loc
        * name
        * (name * ty) list
        * expr (* exception name(x : t, .., x : t) = e *)
    | Try of
        loc * expr * (pattern * expr) list (* try expr with {p -> e, .. } *)
    | Raise of loc * expr (* raise e *)

  and list_comp_clause =
    | DrawClause of pattern * expr (* p <- e *)
    | FilterClause of expr (* e *)

  and interpolation_component =
    | StringComponent of loc * string
    | Interpolation of loc * expr list

  module MExpr = struct
    type t = module_expr

    let rec pretty = function
      | Import (_, path) -> "import \"" ^ path ^ "\""
      | ModVar (_, var) -> pretty_name var
      | SubModule (_, mod_expr, name) ->
          pretty mod_expr ^ "." ^ pretty_name name

    let get_loc = function
      | ModVar (loc, _)
      | SubModule (loc, _, _) ->
          loc
      | Import (ext, _) -> import_ext_loc ext
  end

  type flag_args =
    | Varargs of name
    | Switch of name
    | Named of name list
    | NamedDefault of (name * string) list

  type flag_def = {
    flags : string list;
    args : flag_args;
    description : string option;
  }

  (** WARNING: SHOULD YOU EVER FALL TO THE TEMPTATION OF MAKING
      export_item DIVERGE BETWEEN Renamed AND Typed, **UPDATE typecheck_exports IN types.ml** 
      IF YOU DON'T, HAVOC WILL ENSUE AS EXPORT ITEMS WILL BE ALIENATED FROM THEIR ORIGIN
      AND ENSLAVED AS DIFFERENT TYPES! 
      DARK MAGIC WILL ENGULF THEIR SOULS AND BREAK YOUR INVARIANTS.
      SEGMENTATION FAULTS WILL ROAM THE LAND! TURN BACK WHILE YOU CAN! *)
  type export_item =
    | ExportVal of loc * name
    | ExportConstructor of ExportConstructorExt.t * name

  type header = {
    usage : string option;
    description : string option;
    options : flag_def list;
    exports : export_item list;
  }

  type pretty_type_options = {
    pretty_unif : ty Typeref.t -> name -> string;
    pretty_skol : Unique.t -> Typeref.level -> name -> string;
  }

  let rec pretty_type_with options ty =
    let pretty_type = pretty_type_with options in
    match Ty.normalize_unif ty with
    | Forall (var, ty) -> "∀" ^ pretty_name var ^ ". " ^ pretty_type ty
    | Fun (args, res) ->
        "("
        ^ String.concat ", " (List.map pretty_type args)
        ^ ") -> " ^ pretty_type res
    | TyConstructor (name, []) -> pretty_name name
    | TyConstructor (name, args) ->
        pretty_name name ^ "("
        ^ String.concat ", " (List.map pretty_type args)
        ^ ")"
    | TypeAlias (name, []) -> pretty_name name
    | TypeAlias (name, args) ->
        pretty_name name ^ "("
        ^ String.concat ", " (List.map pretty_type args)
        ^ ")"
    | ModSubscriptTyCon (_ext, mod_name, name, args) ->
        pretty_name mod_name ^ "." ^ pretty_name name ^ "("
        ^ String.concat ", " (List.map pretty_type args)
        ^ ")"
    | TyVar var -> pretty_name var
    | Unif (typeref, name) -> options.pretty_unif typeref name
    | Skol (unique, level, name) -> options.pretty_skol unique level name
    | Number -> "Number"
    | Bool -> "Bool"
    | String -> "String"
    | Exception -> "Exception"
    | Tuple tys ->
        "("
        ^ String.concat ", " (Array.to_list (Array.map pretty_type tys))
        ^ ")"
    | List ty -> "List(" ^ pretty_type ty ^ ")"
    | Promise ty -> "Promise(" ^ pretty_type ty ^ ")"
    | Ref ty -> "Ref(" ^ pretty_type ty ^ ")"
    (* There is no difference between empty records and unit, so to make typechecking easier,
       this type is represented as an empty record, but for the sake of error messages, it is printed as '()' *)
    | RecordClosed [||] -> "()"
    | RecordClosed fields ->
        "{ "
        ^ String.concat ", "
            (Array.to_list
               (Array.map (fun (x, ty) -> x ^ " : " ^ pretty_type ty) fields))
        ^ " }"
    | RecordUnif (fields, (typeref, name)) ->
        "{ "
        ^ String.concat ", "
            (Array.to_list
               (Array.map (fun (x, ty) -> x ^ " : " ^ pretty_type ty) fields))
        ^ " | "
        ^ pretty_type (Unif (typeref, name))
        ^ " }"
    | RecordSkol (fields, (u, level, name)) ->
        "{ "
        ^ String.concat ", "
            (Array.to_list
               (Array.map (fun (x, ty) -> x ^ " : " ^ pretty_type ty) fields))
        ^ " | "
        ^ pretty_type (Skol (u, level, name))
        ^ " }"
    | RecordVar (fields, name) ->
        "{ "
        ^ String.concat ", "
            (Array.to_list
               (Array.map (fun (x, ty) -> x ^ " : " ^ pretty_type ty) fields))
        ^ " | " ^ pretty_name name ^ " }"
    | VariantClosed fields ->
        "< "
        ^ String.concat ", "
            (Array.to_list
               (Array.map
                  (fun (x, tys) ->
                    x ^ "("
                    ^ String.concat ", " (List.map pretty_type tys)
                    ^ ")")
                  fields))
        ^ " >"
    | VariantUnif (fields, (typeref, name)) ->
        "< "
        ^ String.concat ", "
            (Array.to_list
               (Array.map
                  (fun (x, tys) ->
                    x ^ "("
                    ^ String.concat ", " (List.map pretty_type tys)
                    ^ ")")
                  fields))
        ^ " | "
        ^ pretty_type (Unif (typeref, name))
        ^ " >"
    | VariantSkol (fields, (u, level, name)) ->
        "< "
        ^ String.concat ", "
            (Array.to_list
               (Array.map
                  (fun (x, tys) ->
                    x ^ "("
                    ^ String.concat ", " (List.map pretty_type tys)
                    ^ ")")
                  fields))
        ^ " | "
        ^ pretty_type (Skol (u, level, name))
        ^ " >"
    | VariantVar (fields, name) ->
        "< "
        ^ String.concat ", "
            (Array.to_list
               (Array.map
                  (fun (x, tys) ->
                    x ^ "("
                    ^ String.concat ", " (List.map pretty_type tys)
                    ^ ")")
                  fields))
        ^ " | " ^ pretty_name name ^ " >"

  let rec pretty_type ty =
    pretty_type_with (Lazy.force default_pretty_type_config) ty

  (* This is lazy to allow mutual recursion with pretty_type*)
  and default_pretty_type_config =
    lazy
      {
        pretty_unif =
          begin
            fun typeref name ->
              match Typeref.get typeref with
              | Unbound level ->
                  if Config.print_levels () then
                    "?" ^ pretty_name name ^ "$"
                    ^ Unique.display (Typeref.get_unique typeref)
                    ^ "[" ^ Typeref.pretty_level level ^ "]"
                  else
                    "?" ^ pretty_name name ^ "$"
                    ^ Unique.display (Typeref.get_unique typeref)
              | Bound ty ->
                  if Config.print_subst_unif_vars () then
                    "?" ^ pretty_name name ^ "$"
                    ^ Unique.display (Typeref.get_unique typeref)
                    ^ "[= " ^ pretty_type ty ^ "]"
                  else pretty_type ty
          end;
        pretty_skol =
          begin
            fun unique level name ->
              if Config.print_levels () then
                pretty_name name ^ "@" ^ Unique.display unique ^ "["
                ^ Typeref.pretty_level level ^ "]"
              else pretty_name name ^ "@" ^ Unique.display unique
          end;
      }

  let rec pretty_pattern = function
    | VarPat (_, x) -> pretty_name x
    | AsPat (_, pattern, name) ->
        "(" ^ pretty_pattern pattern ^ " as " ^ pretty_name name ^ ")"
    | ConsPat (_, x, xs) ->
        "(" ^ pretty_pattern x ^ ") :: (" ^ pretty_pattern xs ^ ")"
    | ListPat (_, pats) ->
        "[" ^ String.concat ", " (List.map pretty_pattern pats) ^ "]"
    | TuplePat (_, pats) ->
        "(" ^ String.concat ", " (List.map pretty_pattern pats) ^ ")"
    | NumPat (_, f) -> Float.to_string f
    | StringPat (_, literal) -> "\"" ^ literal ^ "\""
    | BoolPat (_, literal) -> string_of_bool literal
    | OrPat (_, p1, p2) ->
        "(" ^ pretty_pattern p1 ^ " | " ^ pretty_pattern p2 ^ ")"
    | TypePat (_, pat, ty) ->
        "(" ^ pretty_pattern pat ^ " : " ^ pretty_type ty ^ ")"
    | DataPat (_, name, pattern) ->
        pretty_name name ^ "(" ^ pretty_pattern pattern ^ ")"
    | VariantPat (_, name, patterns) ->
        "`" ^ name ^ "("
        ^ String.concat ", " (List.map pretty_pattern patterns)
        ^ ")"
    | ExceptionDataPat (_, name, patterns) ->
        pretty_name name ^ "("
        ^ String.concat ", " (List.map pretty_pattern patterns)
        ^ ")"

  let rec pretty = function
    | Var (_, x) -> pretty_name x
    | DataConstructor (_, x) -> pretty_name x
    | ExceptionConstructor (_, x) -> pretty_name x
    | VariantConstructor (_, x, args) ->
        "`" ^ x ^ "(" ^ String.concat ", " (List.map pretty args) ^ ")"
    | ModSubscriptDataCon (_, _, mod_name, name) ->
        pretty_name mod_name ^ "." ^ pretty_name name
    | App (_, f, args) ->
        pretty f ^ "(" ^ String.concat ", " (List.map pretty args) ^ ")"
    | Lambda (_, params, e) ->
        "\\("
        ^ String.concat ", " (List.map pretty_pattern params)
        ^ ") -> " ^ pretty e
    | StringLit (_, l) -> "\"" ^ l ^ "\""
    | NumLit (_, f) -> string_of_float f
    | BoolLit (_, b) -> string_of_bool b
    | UnitLit _ -> "()"
    | ListLit (_, exprs) ->
        "[" ^ String.concat ", " (List.map pretty exprs) ^ "]"
    | TupleLit (_, exprs) ->
        "(" ^ String.concat ", " (List.map pretty exprs) ^ ")"
    | RecordLit (_, kvs) ->
        "{"
        ^ String.concat ", " (List.map (fun (k, e) -> k ^ " = " ^ pretty e) kvs)
        ^ "}"
    | StringInterpolation (_, components) ->
        let pretty_component = function
          | StringComponent (_, str) -> str
          | Interpolation (_, exprs) ->
              "${" ^ String.concat "; " (List.map pretty exprs) ^ "}"
        in
        "\"" ^ String.concat "" (List.map pretty_component components) ^ "\""
    | Subscript (_, expr, key) -> "(" ^ pretty expr ^ ")." ^ key
    | ModSubscript (_, mod_name, key_name) ->
        pretty_name mod_name ^ "." ^ pretty_name key_name
    | RecordUpdate (_, expr, kvs) ->
        "#{" ^ pretty expr ^ " with "
        ^ String.concat ", " (List.map (fun (k, e) -> k ^ " = " ^ pretty e) kvs)
        ^ "}"
    | RecordExtension (_, expr, kvs) ->
        "#{" ^ pretty expr ^ " extend "
        ^ String.concat ", " (List.map (fun (k, e) -> k ^ " = " ^ pretty e) kvs)
        ^ "}"
    | DynLookup (_, mexpr, kexpr) ->
        "(" ^ pretty mexpr ^ ")[" ^ pretty kexpr ^ "]"
    | BinOp (_, e1, Add, e2) -> "(" ^ pretty e1 ^ " + " ^ pretty e2 ^ ")"
    | BinOp (_, e1, Sub, e2) -> "(" ^ pretty e1 ^ " - " ^ pretty e2 ^ ")"
    | BinOp (_, e1, Mul, e2) -> "(" ^ pretty e1 ^ " * " ^ pretty e2 ^ ")"
    | BinOp (_, e1, Div, e2) -> "(" ^ pretty e1 ^ " / " ^ pretty e2 ^ ")"
    | BinOp (_, e1, Concat, e2) -> "(" ^ pretty e1 ^ " .. " ^ pretty e2 ^ ")"
    | BinOp (_, e1, Cons, e2) -> "(" ^ pretty e1 ^ " :: " ^ pretty e2 ^ ")"
    | BinOp (_, e1, Equals, e2) -> "(" ^ pretty e1 ^ " == " ^ pretty e2 ^ ")"
    | BinOp (_, e1, NotEquals, e2) -> "(" ^ pretty e1 ^ " != " ^ pretty e2 ^ ")"
    | BinOp (_, e1, LE, e2) -> "(" ^ pretty e1 ^ " <= " ^ pretty e2 ^ ")"
    | BinOp (_, e1, GE, e2) -> "(" ^ pretty e1 ^ " >= " ^ pretty e2 ^ ")"
    | BinOp (_, e1, LT, e2) -> "(" ^ pretty e1 ^ " <  " ^ pretty e2 ^ ")"
    | BinOp (_, e1, GT, e2) -> "(" ^ pretty e1 ^ " >  " ^ pretty e2 ^ ")"
    | BinOp (_, e1, Or, e2) -> "(" ^ pretty e1 ^ " || " ^ pretty e2 ^ ")"
    | BinOp (_, e1, And, e2) -> "(" ^ pretty e1 ^ " && " ^ pretty e2 ^ ")"
    | Not (_, e) -> "(not " ^ pretty e ^ ")"
    | Range (_, e1, e2) -> "[" ^ pretty e1 ^ " .. " ^ pretty e2 ^ "]"
    | ListComp (_, e, clauses) ->
        let pretty_list_comp = function
          | DrawClause (x, e) -> pretty_pattern x ^ " <- " ^ pretty e
          | FilterClause e -> pretty e
        in
        "[" ^ pretty e ^ " | "
        ^ String.concat ", " (List.map pretty_list_comp clauses)
    | If (_, e1, e2, e3) ->
        "if " ^ pretty e1 ^ " then " ^ pretty e2 ^ " else " ^ pretty e3
    | Seq (_, exprs) -> "{ " ^ String.concat "; " (List.map pretty exprs) ^ "}"
    | LetSeq (_, x, e) -> "let " ^ pretty_pattern x ^ " = " ^ pretty e
    | LetRecSeq (_, None, x, xs, e) ->
        "let " ^ pretty_name x ^ "("
        ^ String.concat ", " (List.map pretty_pattern xs)
        ^ ") = " ^ pretty e
    | LetRecSeq (_, Some ty, x, xs, e) ->
        "let " ^ pretty_name x ^ " : " ^ pretty_type ty ^ "; let "
        ^ pretty_name x ^ "("
        ^ String.concat ", " (List.map pretty_pattern xs)
        ^ ") = " ^ pretty e
    | LetEnvSeq (_, x, e) -> "let $" ^ x ^ " = " ^ pretty e
    | LetDataSeq (_, varname, [], ty) ->
        "data " ^ pretty_name varname ^ " = " ^ pretty_type ty
    | LetDataSeq (_, varname, params, ty) ->
        "data " ^ pretty_name varname ^ "("
        ^ String.concat ", " (List.map pretty_name params)
        ^ ") = " ^ pretty_type ty
    | LetTypeSeq (_, varname, [], ty) ->
        "type " ^ pretty_name varname ^ " = " ^ pretty_type ty
    | LetTypeSeq (_, varname, params, ty) ->
        "type " ^ pretty_name varname ^ "("
        ^ String.concat ", " (List.map pretty_name params)
        ^ ") = " ^ pretty_type ty
    | ProgCall (_, prog, args) ->
        "!" ^ prog ^ " " ^ String.concat " " (List.map pretty args)
    | Pipe (_, exprs) -> String.concat " | " (List.map pretty exprs)
    | EnvVar (_, x) -> "$" ^ x
    | Async (_, expr) -> "async " ^ pretty expr
    | Await (_, expr) -> "await " ^ pretty expr
    | Match (_, expr, pats) ->
        "match " ^ pretty expr ^ " {" ^ "\n    "
        ^ String.concat "\n    "
            (List.map (fun (p, e) -> pretty_pattern p ^ " -> " ^ pretty e) pats)
        ^ "\n}"
    | LetModuleSeq (_, name, mexpr) ->
        "module " ^ pretty_name name ^ " = " ^ MExpr.pretty mexpr
    | Ascription (_, expr, ty) ->
        "(" ^ pretty expr ^ " : " ^ pretty_type ty ^ ")"
    | Unwrap (_, expr) -> pretty expr ^ "!"
    | MakeRef (_, expr) -> "ref " ^ pretty expr ^ ""
    | Assign (_, x, e) -> pretty x ^ " = " ^ pretty e
    | LetExceptionSeq (_, name, params, message_expr) ->
        "exception " ^ pretty_name name ^ "("
        ^ String.concat ","
            (List.map
               (fun (param, ty) -> pretty_name param ^ " : " ^ pretty_type ty)
               params)
        ^ ") = " ^ pretty message_expr
    | Try (_, try_expr, branches) ->
        "try " ^ pretty try_expr ^ " with {" ^ "\n    "
        ^ String.concat "\n    "
            (List.map
               (fun (pattern, expr) ->
                 pretty_pattern pattern ^ " -> " ^ pretty expr)
               branches)
        ^ "\n}"
    | Raise (_, expr) -> "raise " ^ pretty expr

  let pretty_list (exprs : expr list) : string =
    List.fold_right (fun x r -> pretty x ^ "\n" ^ r) exprs ""

  let get_loc = function
    | Var (ext, _) -> VarExt.loc ext
    | DataConstructor (ext, _) -> DataConExt.loc ext
    | Subscript (ext, _, _) -> SubscriptExt.loc ext
    | ModSubscript (ext, _, _) -> ModSubscriptExt.loc ext
    | LetRecSeq (ext, _, _, _, _) -> LetRecExt.loc ext
    | ExceptionConstructor (loc, _)
    | VariantConstructor (loc, _, _)
    | ModSubscriptDataCon (_, loc, _, _)
    | App (loc, _, _)
    | Lambda (loc, _, _)
    | StringLit (loc, _)
    | NumLit (loc, _)
    | StringInterpolation (loc, _)
    | BoolLit (loc, _)
    | UnitLit loc
    | ListLit (loc, _)
    | TupleLit (loc, _)
    | RecordLit (loc, _)
    | RecordUpdate (loc, _, _)
    | RecordExtension (loc, _, _)
    | DynLookup (loc, _, _)
    | BinOp (loc, _, _, _)
    | Not (loc, _)
    | Range (loc, _, _)
    | ListComp (loc, _, _)
    | If (loc, _, _, _)
    | Seq (loc, _)
    | LetSeq (loc, _, _)
    | LetEnvSeq (loc, _, _)
    | LetDataSeq (loc, _, _, _)
    | LetTypeSeq (loc, _, _, _)
    | Assign (loc, _, _)
    | ProgCall (loc, _, _)
    | Pipe (loc, _)
    | EnvVar (loc, _)
    | Async (loc, _)
    | Await (loc, _)
    | Match (loc, _, _)
    | LetModuleSeq (loc, _, _)
    | Ascription (loc, _, _)
    | Unwrap (loc, _)
    | MakeRef (loc, _)
    | LetExceptionSeq (loc, _, _, _)
    | Try (loc, _, _)
    | Raise (loc, _) ->
        loc

  let get_pattern_loc = function
    | VarPat (ext, _) -> VarPatExt.loc ext
    | VariantPat (ext, _, _) -> VariantPatExt.loc ext
    | AsPat (loc, _, _)
    | ConsPat (loc, _, _)
    | ListPat (loc, _)
    | TuplePat (loc, _)
    | NumPat (loc, _)
    | StringPat (loc, _)
    | BoolPat (loc, _)
    | OrPat (loc, _, _)
    | TypePat (loc, _, _)
    | DataPat (loc, _, _)
    | ExceptionDataPat (loc, _, _) ->
        loc

  module Traversal = struct
    (* The traversal system here is similar to the one used in ppx_lib for the OCaml AST.
       Though unlike that one, there are not separate traversal classes for map, fold, etc here.
       Instead, every traversal performs a combined map/fold operation.

       The syntax tree is always traversed in a bottom-up manner.
    *)

    let traverse_list :
          'state 'node.
          ('state -> 'node -> 'node * 'state) ->
          'state ->
          'node list ->
          'node list * 'state =
     fun traversal state exprs ->
      List.fold_right
        (fun expr (exprs, state) ->
          let expr, state = traversal state expr in
          (expr :: exprs, state))
        exprs ([], state)

    let traverse_option :
          'state 'node.
          ('state -> 'node -> 'node * 'state) ->
          'state ->
          'node option ->
          'node option * 'state =
     fun traversal state expr_opt ->
      match expr_opt with
      | None -> (None, state)
      | Some expr ->
          let expr, state = traversal state expr in
          (Some expr, state)

    class ['state] traversal =
      object (self)
        method expr : 'state -> expr -> expr * 'state =
          fun state expr -> (expr, state)

        method pattern : 'state -> pattern -> pattern * 'state =
          fun state pattern -> (pattern, state)

        method ty : 'state -> ty -> ty * 'state = fun state ty -> (ty, state)

        method name : 'state -> name -> name * 'state =
          fun state name -> (name, state)

        method module_expr : 'state -> module_expr -> module_expr * 'state =
          fun state module_expr -> (module_expr, state)

        method list_comp_clause
            : 'state -> list_comp_clause -> list_comp_clause * 'state =
          fun state list_comp_clause -> (list_comp_clause, state)

        method traverse_expr : 'state -> expr -> expr * 'state =
          fun state expression ->
            let transformed, state =
              match expression with
              (* Non-recursive cases (that don't mention another traversable node type) *)
              | StringLit _
              | NumLit _
              | BoolLit _
              | UnitLit _
              | EnvVar _ ->
                  (expression, state)
              (* Recursive boilerplate *)
              | Var (ext, name) ->
                  let ext, state =
                    VarExt.traverse_ty state self#traverse_type ext
                  in
                  let name, state = self#traverse_name state name in
                  (Var (ext, name), state)
              | DataConstructor (loc, name) ->
                  let name, state = self#traverse_name state name in
                  (DataConstructor (loc, name), state)
              | ExceptionConstructor (loc, name) ->
                  let name, state = self#traverse_name state name in
                  (ExceptionConstructor (loc, name), state)
              | VariantConstructor (loc, name, args) ->
                  let args, state =
                    traverse_list self#traverse_expr state args
                  in
                  (VariantConstructor (loc, name, args), state)
              | ModSubscriptDataCon (ext, loc, mod_name, name) ->
                  let mod_name, state = self#traverse_name state mod_name in
                  let name, state = self#traverse_name state name in
                  (ModSubscriptDataCon (ext, loc, mod_name, name), state)
              | App (loc, fun_expr, arg_exprs) ->
                  let fun_expr, state = self#traverse_expr state fun_expr in
                  let arg_exprs, state =
                    traverse_list self#traverse_expr state arg_exprs
                  in
                  (App (loc, fun_expr, arg_exprs), state)
              | Lambda (loc, patterns, body) ->
                  let patterns, state =
                    traverse_list self#traverse_pattern state patterns
                  in
                  let body, state = self#traverse_expr state body in
                  (Lambda (loc, patterns, body), state)
              | ListLit (loc, exprs) ->
                  let exprs, state =
                    traverse_list self#traverse_expr state exprs
                  in
                  (ListLit (loc, exprs), state)
              | TupleLit (loc, exprs) ->
                  let exprs, state =
                    traverse_list self#traverse_expr state exprs
                  in
                  (TupleLit (loc, exprs), state)
              | RecordLit (loc, fields) ->
                  let fields, state =
                    traverse_list
                      (fun state (field, expr) ->
                        let expr, state = self#traverse_expr state expr in
                        ((field, expr), state))
                      state fields
                  in
                  (RecordLit (loc, fields), state)
              | StringInterpolation (loc, components) ->
                  let components, state =
                    traverse_list
                      (fun state component ->
                        match component with
                        | StringComponent _ -> (component, state)
                        | Interpolation (loc, exprs) ->
                            let exprs, state =
                              traverse_list self#traverse_expr state exprs
                            in
                            (Interpolation (loc, exprs), state))
                      state components
                  in
                  (StringInterpolation (loc, components), state)
              | Subscript (loc, expr, field) ->
                  let expr, state = self#traverse_expr state expr in
                  (Subscript (loc, expr, field), state)
              | RecordUpdate (loc, expr, updates) ->
                  let expr, state = self#traverse_expr state expr in
                  let updates, state =
                    traverse_list
                      (fun state (field, expr) ->
                        let expr, state = self#traverse_expr state expr in
                        ((field, expr), state))
                      state updates
                  in
                  (RecordUpdate (loc, expr, updates), state)
              | RecordExtension (loc, expr, updates) ->
                  let expr, state = self#traverse_expr state expr in
                  let updates, state =
                    traverse_list
                      (fun state (field, expr) ->
                        let expr, state = self#traverse_expr state expr in
                        ((field, expr), state))
                      state updates
                  in
                  (RecordExtension (loc, expr, updates), state)
              | DynLookup (loc, expr, field_expr) ->
                  let expr, state = self#traverse_expr state expr in
                  let field_expr, state = self#traverse_expr state field_expr in
                  (DynLookup (loc, expr, field_expr), state)
              | BinOp (loc, left_expr, op, right_expr) ->
                  let left_expr, state = self#traverse_expr state left_expr in
                  let right_expr, state = self#traverse_expr state right_expr in
                  (BinOp (loc, left_expr, op, right_expr), state)
              | Not (loc, expr) ->
                  let expr, state = self#traverse_expr state expr in
                  (Not (loc, expr), state)
              | Range (loc, start_expr, end_expr) ->
                  let start_expr, state = self#traverse_expr state start_expr in
                  let end_expr, state = self#traverse_expr state end_expr in
                  (Range (loc, start_expr, end_expr), state)
              | ListComp (loc, expr, clauses) ->
                  let expr, state = self#traverse_expr state expr in
                  let clauses, state =
                    traverse_list self#traverse_list_comp_clause state clauses
                  in
                  (ListComp (loc, expr, clauses), state)
              | If (loc, condition_expr, then_expr, else_expr) ->
                  let condition_expr, state =
                    self#traverse_expr state condition_expr
                  in
                  let then_expr, state = self#traverse_expr state then_expr in
                  let else_expr, state = self#traverse_expr state else_expr in
                  (If (loc, condition_expr, then_expr, else_expr), state)
              | Seq (loc, exprs) ->
                  let exprs, state =
                    traverse_list self#traverse_expr state exprs
                  in
                  (Seq (loc, exprs), state)
              | LetSeq (loc, pattern, expr) ->
                  let pattern, state = self#traverse_pattern state pattern in
                  let expr, state = self#traverse_expr state expr in
                  (LetSeq (loc, pattern, expr), state)
              | LetRecSeq
                  (loc, maybe_typesig, fun_name, param_patterns, body_expr) ->
                  let maybe_typesig, state =
                    match maybe_typesig with
                    | None -> (None, state)
                    | Some ty ->
                        let ty, state = self#traverse_type state ty in
                        (Some ty, state)
                  in
                  let fun_name, state = self#traverse_name state fun_name in
                  let param_patterns, state =
                    traverse_list self#traverse_pattern state param_patterns
                  in
                  let body_expr, state = self#traverse_expr state body_expr in
                  ( LetRecSeq
                      (loc, maybe_typesig, fun_name, param_patterns, body_expr),
                    state )
              | LetEnvSeq (loc, env_var, expr) ->
                  let expr, state = self#traverse_expr state expr in
                  (LetEnvSeq (loc, env_var, expr), state)
              | LetDataSeq (loc, data_name, params, ty) ->
                  let data_name, state = self#traverse_name state data_name in
                  let params, state =
                    traverse_list self#traverse_name state params
                  in
                  let ty, state = self#traverse_type state ty in
                  (LetDataSeq (loc, data_name, params, ty), state)
              | LetTypeSeq (loc, data_name, params, ty) ->
                  let data_name, state = self#traverse_name state data_name in
                  let params, state =
                    traverse_list self#traverse_name state params
                  in
                  let ty, state = self#traverse_type state ty in
                  (LetTypeSeq (loc, data_name, params, ty), state)
              | Assign (loc, place_expr, expr) ->
                  let place_expr, state = self#traverse_expr state place_expr in
                  let expr, state = self#traverse_expr state expr in
                  (Assign (loc, place_expr, expr), state)
              | ProgCall (loc, progname, exprs) ->
                  let exprs, state =
                    traverse_list self#traverse_expr state exprs
                  in
                  (ProgCall (loc, progname, exprs), state)
              | Pipe (loc, exprs) ->
                  let exprs, state =
                    traverse_list self#traverse_expr state exprs
                  in
                  (Pipe (loc, exprs), state)
              | Async (loc, expr) ->
                  let expr, state = self#traverse_expr state expr in
                  (Async (loc, expr), state)
              | Await (loc, expr) ->
                  let expr, state = self#traverse_expr state expr in
                  (Await (loc, expr), state)
              | Match (loc, scrutinee_expr, branch_exprs) ->
                  let scrutinee_expr, state =
                    self#traverse_expr state scrutinee_expr
                  in
                  let branch_exprs, state =
                    traverse_list
                      (fun state (pattern, expr) ->
                        let pattern, state =
                          self#traverse_pattern state pattern
                        in
                        let expr, state = self#traverse_expr state expr in
                        ((pattern, expr), state))
                      state branch_exprs
                  in
                  (Match (loc, scrutinee_expr, branch_exprs), state)
              | LetModuleSeq (loc, name, mod_expr) ->
                  let name, state = self#traverse_name state name in
                  let mod_expr, state =
                    self#traverse_module_expr state mod_expr
                  in
                  (LetModuleSeq (loc, name, mod_expr), state)
              | Ascription (loc, expr, ty) ->
                  let expr, state = self#traverse_expr state expr in
                  let ty, state = self#traverse_type state ty in
                  (Ascription (loc, expr, ty), state)
              | ModSubscript (ext, mod_name, field_name) ->
                  let mod_name, state = self#traverse_name state mod_name in
                  let field_name, state = self#traverse_name state field_name in
                  (ModSubscript (ext, mod_name, field_name), state)
              | Unwrap (loc, expr) ->
                  let expr, state = self#traverse_expr state expr in
                  (Unwrap (loc, expr), state)
              | MakeRef (loc, expr) ->
                  let expr, state = self#traverse_expr state expr in
                  (MakeRef (loc, expr), state)
              | LetExceptionSeq (loc, name, params, message_expr) ->
                  let name, state = self#traverse_name state name in
                  let params, state =
                    traverse_list
                      (fun state (name, ty) ->
                        let name, state = self#traverse_name state name in
                        let ty, state = self#traverse_type state ty in
                        ((name, ty), state))
                      state params
                  in
                  let message_expr, state =
                    self#traverse_expr state message_expr
                  in
                  (LetExceptionSeq (loc, name, params, message_expr), state)
              | Try (loc, try_expr, branches) ->
                  let try_expr, state = self#traverse_expr state try_expr in
                  let branches, state =
                    traverse_list
                      (fun state (pattern, expr) ->
                        let pattern, state =
                          self#traverse_pattern state pattern
                        in
                        let expr, state = self#traverse_expr state expr in
                        ((pattern, expr), state))
                      state branches
                  in
                  (Try (loc, try_expr, branches), state)
              | Raise (loc, expr) ->
                  let expr, state = self#traverse_expr state expr in
                  (Raise (loc, expr), state)
            in
            self#expr state transformed

        method traverse_pattern : 'state -> pattern -> pattern * 'state =
          fun state pattern ->
            let transformed, state =
              match pattern with
              (* Non-recursive *)
              | NumPat (loc, num) -> (NumPat (loc, num), state)
              | StringPat (loc, num) -> (StringPat (loc, num), state)
              | BoolPat (loc, bool) -> (BoolPat (loc, bool), state)
              (* Recursive *)
              | VarPat (loc, name) ->
                  let name, state = self#traverse_name state name in
                  (VarPat (loc, name), state)
              | AsPat (loc, pattern, name) ->
                  let pattern, state = self#traverse_pattern state pattern in
                  let name, state = self#traverse_name state name in
                  (AsPat (loc, pattern, name), state)
              | ConsPat (loc, head_pattern, tail_pattern) ->
                  let head_pattern, state =
                    self#traverse_pattern state head_pattern
                  in
                  let tail_pattern, state =
                    self#traverse_pattern state tail_pattern
                  in
                  (ConsPat (loc, head_pattern, tail_pattern), state)
              | ListPat (loc, patterns) ->
                  let patterns, state =
                    traverse_list self#traverse_pattern state patterns
                  in
                  (ListPat (loc, patterns), state)
              | TuplePat (loc, patterns) ->
                  let patterns, state =
                    traverse_list self#traverse_pattern state patterns
                  in
                  (TuplePat (loc, patterns), state)
              | OrPat (loc, left_pattern, right_pattern) ->
                  let left_pattern, state =
                    self#traverse_pattern state left_pattern
                  in
                  let right_pattern, state =
                    self#traverse_pattern state right_pattern
                  in
                  (OrPat (loc, left_pattern, right_pattern), state)
              | TypePat (loc, pattern, ty) ->
                  let pattern, state = self#traverse_pattern state pattern in
                  let ty, state = self#traverse_type state ty in
                  (TypePat (loc, pattern, ty), state)
              | DataPat (loc, constructor_name, pattern) ->
                  let constructor_name, state =
                    self#traverse_name state constructor_name
                  in
                  let patterns, state = self#traverse_pattern state pattern in
                  (DataPat (loc, constructor_name, patterns), state)
              | VariantPat (loc, unqualified_name, patterns) ->
                  let patterns, state =
                    traverse_list self#traverse_pattern state patterns
                  in
                  (VariantPat (loc, unqualified_name, patterns), state)
              | ExceptionDataPat (loc, name, patterns) ->
                  let name, state = self#traverse_name state name in
                  let patterns, state =
                    traverse_list self#traverse_pattern state patterns
                  in
                  (ExceptionDataPat (loc, name, patterns), state)
            in
            self#pattern state transformed

        method traverse_type : 'state -> ty -> ty * 'state =
          fun state ty ->
            let transformed, state =
              match ty with
              (* Non-recursive cases *)
              | Number
              | Bool
              | String
              | Exception ->
                  (ty, state)
              (* Recursion *)
              | Forall (var_name, ty) ->
                  let var_name, state = self#traverse_name state var_name in
                  let ty, state = self#traverse_type state ty in
                  (Forall (var_name, ty), state)
              | Fun (dom_types, cod_type) ->
                  let dom_types, state =
                    traverse_list self#traverse_type state dom_types
                  in
                  let cod_type, state = self#traverse_type state cod_type in
                  (Fun (dom_types, cod_type), state)
              | TyVar name ->
                  let name, state = self#traverse_name state name in
                  (TyVar name, state)
              | TyConstructor (name, arg_types) ->
                  let name, state = self#traverse_name state name in
                  let arg_types, state =
                    traverse_list self#traverse_type state arg_types
                  in
                  (TyConstructor (name, arg_types), state)
              | TypeAlias (name, arg_types) ->
                  let name, state = self#traverse_name state name in
                  let arg_types, state =
                    traverse_list self#traverse_type state arg_types
                  in
                  (TypeAlias (name, arg_types), state)
              | ModSubscriptTyCon (ext, mod_name, name, arg_types) ->
                  let mod_name, state = self#traverse_name state mod_name in
                  let name, state = self#traverse_name state name in
                  let arg_types, state =
                    traverse_list self#traverse_type state arg_types
                  in
                  (ModSubscriptTyCon (ext, mod_name, name, arg_types), state)
              | Unif (typeref, name) -> begin
                  match Typeref.get typeref with
                  | Unbound level ->
                      let name, state = self#traverse_name state name in
                      (Unif (typeref, name), state)
                  (* If this unif var has been substituted, we completely ignore it. *)
                  | Bound ty -> self#traverse_type state ty
                end
              | Skol (unique, level, name) ->
                  let name, state = self#traverse_name state name in
                  (Skol (unique, level, name), state)
              | Tuple tys ->
                  let tys, state =
                    traverse_list self#traverse_type state (Array.to_list tys)
                  in
                  (Tuple (Array.of_list tys), state)
              | List ty ->
                  let ty, state = self#traverse_type state ty in
                  (List ty, state)
              | Promise ty ->
                  let ty, state = self#traverse_type state ty in
                  (Promise ty, state)
              | Ref ty ->
                  let ty, state = self#traverse_type state ty in
                  (Ref ty, state)
              | RecordClosed fields ->
                  let fields, state =
                    traverse_list
                      (fun state (field, ty) ->
                        let ty, state = self#traverse_type state ty in
                        ((field, ty), state))
                      state (Array.to_list fields)
                  in
                  (RecordClosed (Array.of_list fields), state)
              | VariantClosed fields ->
                  let fields, state =
                    traverse_list
                      (fun state (field, tys) ->
                        let tys, state =
                          traverse_list self#traverse_type state tys
                        in
                        ((field, tys), state))
                      state (Array.to_list fields)
                  in
                  (VariantClosed (Array.of_list fields), state)
              | RecordVar (fields, var) ->
                  let fields, state =
                    traverse_list
                      (fun state (field, ty) ->
                        let ty, state = self#traverse_type state ty in
                        ((field, ty), state))
                      state (Array.to_list fields)
                  in
                  (* Just as in Ty.transform, we treat record extension variables as freestanding
                     types and properly merge the transformed results back to give a single record type
                     TODO: Maybe this should be configurable? *)
                  let var, state = self#traverse_type state (TyVar var) in
                  (Ty.replace_record_extension (Array.of_list fields) var, state)
              | VariantVar (fields, var) ->
                  let fields, state =
                    traverse_list
                      (fun state (field, tys) ->
                        let tys, state =
                          traverse_list self#traverse_type state tys
                        in
                        ((field, tys), state))
                      state (Array.to_list fields)
                  in
                  let var, state = self#traverse_type state (TyVar var) in
                  ( Ty.replace_variant_extension (Array.of_list fields) var,
                    state )
              | RecordUnif (fields, (typeref, var)) ->
                  (* TODO: We *should* probably pretend like this constructor does not exist if the
                     unification variable is replaced. Not quite sure how to do that efficiently though *)
                  let fields, state =
                    traverse_list
                      (fun state (field, ty) ->
                        let ty, state = self#traverse_type state ty in
                        ((field, ty), state))
                      state (Array.to_list fields)
                  in
                  let var, state =
                    self#traverse_type state (Unif (typeref, var))
                  in
                  (Ty.replace_record_extension (Array.of_list fields) var, state)
              | VariantUnif (fields, (typeref, var)) ->
                  (* TODO: We *should* probably pretend like this constructor does not exist if the
                     unification variable is replaced. Not quite sure how to do that efficiently though *)
                  let fields, state =
                    traverse_list
                      (fun state (field, tys) ->
                        let tys, state =
                          traverse_list self#traverse_type state tys
                        in
                        ((field, tys), state))
                      state (Array.to_list fields)
                  in
                  let var, state =
                    self#traverse_type state (Unif (typeref, var))
                  in
                  ( Ty.replace_variant_extension (Array.of_list fields) var,
                    state )
              | RecordSkol (fields, (unique, level, var)) ->
                  let fields, state =
                    traverse_list
                      (fun state (field, ty) ->
                        let ty, state = self#traverse_type state ty in
                        ((field, ty), state))
                      state (Array.to_list fields)
                  in
                  let var, state =
                    self#traverse_type state (Skol (unique, level, var))
                  in
                  (Ty.replace_record_extension (Array.of_list fields) var, state)
              | VariantSkol (fields, (unique, level, var)) ->
                  let fields, state =
                    traverse_list
                      (fun state (field, tys) ->
                        let tys, state =
                          traverse_list self#traverse_type state tys
                        in
                        ((field, tys), state))
                      state (Array.to_list fields)
                  in
                  let var, state =
                    self#traverse_type state (Skol (unique, level, var))
                  in
                  ( Ty.replace_variant_extension (Array.of_list fields) var,
                    state )
            in
            self#ty state transformed

        method traverse_name : 'state -> name -> name * 'state =
          fun state name -> self#name state name

        method traverse_module_expr
            : 'state -> module_expr -> module_expr * 'state =
          fun state module_expr ->
            let transformed, state =
              match module_expr with
              | Import (loc, path) -> (Import (loc, path), state)
              | ModVar (loc, varname) ->
                  let varname, state = self#traverse_name state varname in
                  (ModVar (loc, varname), state)
              | SubModule (loc, mod_expr, name) ->
                  let mod_expr, state =
                    self#traverse_module_expr state mod_expr
                  in
                  let name, state = self#traverse_name state name in
                  (SubModule (loc, mod_expr, name), state)
            in
            self#module_expr state transformed

        method traverse_list_comp_clause
            : 'state -> list_comp_clause -> list_comp_clause * 'state =
          fun state list_comp_clause ->
            let transformed, state =
              match list_comp_clause with
              | DrawClause (pattern, expr) ->
                  let pattern, state = self#traverse_pattern state pattern in
                  let expr, state = self#traverse_expr state expr in
                  (DrawClause (pattern, expr), state)
              | FilterClause expr ->
                  let expr, state = self#traverse_expr state expr in
                  (FilterClause expr, state)
            in
            self#list_comp_clause state transformed
      end

    let transform_type trans ty =
      let traversal =
        object
          inherit [unit] traversal

          method! ty () ty =
            let ty = trans ty in
            (ty, ())
        end
      in
      fst (traversal#traverse_type () ty)
  end
end
[@@ttg_template]

module ParsedTypeDefinition = MakeTy (struct
  type name = string
  type mod_subscript_tycon_ext = unit
end)

module FullTypeDefinition = MakeTy (struct
  type name = Name.t
  type mod_subscript_tycon_ext = void
end)

module LocOnly = struct
  type t = loc

  let loc loc = loc
  let traverse_ty state _ loc = (loc, state)
end

module LocWithNonTy (NonTy : sig
  type t
end) =
struct
  type t = loc * NonTy.t

  let loc (loc, _) = loc
  let traverse_ty state _ loc = (loc, state)
end

module SubLocation = struct
  type t = {
    main : loc;
    subloc : loc;
  }

  let loc loc = loc.main
  let traverse_ty state _ loc = (loc, state)
end

module WithType = struct
  type t = loc * FullTypeDefinition.ty

  let loc (loc, _) = loc

  let traverse_ty state f (loc, ty) =
    let ty, state = f state ty in
    ((loc, ty), state)
end

module SubLocationWithType = struct
  type t = SubLocation.t * FullTypeDefinition.ty

  let loc (({ main; _ } : SubLocation.t), _) = main

  let traverse_ty state f (loc, ty) =
    let ty, state = f state ty in
    ((loc, ty), state)
end

module Parsed = Template (struct
  module TypeDefinition = ParsedTypeDefinition

  type name = string

  let pretty_name x = x

  type import_ext = loc

  let import_ext_loc x = x

  type mod_subscript_tycon_ext = unit

  module VarExt = LocOnly
  module VarPatExt = LocOnly
  module VariantPatExt = LocOnly
  module DataConExt = LocOnly
  module SubscriptExt = SubLocation
  module ModSubscriptExt = LocOnly
  module LetRecExt = SubLocation
  module ExportConstructorExt = LocOnly
end)
[@@ttg_pass]

module Typed = Template (struct
  module TypeDefinition = FullTypeDefinition

  type name = Name.t

  let pretty_name = Name.pretty

  type import_ext = loc * module_exports * expr list

  let import_ext_loc (loc, _, _) = loc

  type mod_subscript_tycon_ext = void

  module VarExt = WithType
  module VarPatExt = WithType
  module VariantPatExt = WithType
  module DataConExt = WithType
  module SubscriptExt = SubLocationWithType
  module ModSubscriptExt = WithType
  module LetRecExt = SubLocationWithType

  module ExportConstructorExt = LocWithNonTy (struct
    type t =
      [ `Type
      | `Exception
      ]
  end)
end)
[@@ttg_pass]

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

  module VarExt = LocOnly
  module VarPatExt = LocOnly
  module VariantPatExt = LocOnly
  module DataConExt = LocOnly
  module SubscriptExt = SubLocation
  module ModSubscriptExt = LocOnly
  module LetRecExt = SubLocation

  module ExportConstructorExt = LocWithNonTy (struct
    type t =
      [ `Type
      | `Exception
      ]
  end)
end)
[@@ttg_pass]

let coerce_bin_op : Renamed.binop -> Typed.binop = function
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
