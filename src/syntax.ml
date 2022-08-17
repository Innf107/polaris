type loc = Athena.loc
module Loc = Athena.Loc

module Make (Name : sig
  type t

  val pretty : t -> string
end) =
struct
  type name = Name.t

  type ty =
    | Forall of name * ty
    | Var of name
    (* The 'name' is just kept around for error messages but *completely ignored* when typechecking *)
    | Unif of Unique.t * name
    | Number
    | Bool
    | String
    | Tuple of ty array
    | List of ty
    (* TODO: row polymorphism and type classes s*)

  type pattern =
    | VarPat of loc * name
    | ConsPat of loc * pattern * pattern
    | ListPat of loc * pattern list
    | TuplePat of loc * pattern list
    | NumPat of loc * float
    | OrPat of loc * pattern * pattern

  type expr =
    (* Lambda calculus *)
    | Var of loc * name                     (* x *)
    | App of loc * expr * expr list         (* e (e₁, .., eₙ) *)
    | Lambda of loc * pattern list * expr      (* \(p₁, .., pₙ) -> e*)
    (* Literals *)
    | StringLit of loc * string             (* "str" *)
    | NumLit of loc * float                 (* f *)
    | BoolLit of loc * bool                 (* true | false*)
    | UnitLit of loc                        (* () *)
    | NullLit of loc                        (* null *)
    | ListLit of loc * expr list            (* [e, .., e] *)
    | TupleLit of loc * expr list           (* (e, .., e) *)
    | MapLit of loc * (string * expr) list  (* #{x₁: e₁, .., xₙ: eₙ}*)
    (* Map Manipulation *)
    | MapLookup of loc * expr * string      (* e.x *)
    | DynLookup of loc * expr * expr        (* e[e] *)
    (* Common Operations *)
    | Add of loc * expr * expr              (* e + e *)
    | Sub of loc * expr * expr              (* e - e *)
    | Mul of loc * expr * expr              (* e * e *)
    | Div of loc * expr * expr              (* e / e *)
    | Concat of loc * expr * expr           (* e ~ e*)
    | Equals of loc * expr * expr           (* e == e *)
    | NotEquals of loc * expr * expr        (* e != e *)
    | LE of loc * expr * expr               (* e <= e *)
    | GE of loc * expr * expr               (* e >= e *)
    | LT of loc * expr * expr               (* e <  e *)
    | GT of loc * expr * expr               (* e >  e *)

    | Or of loc * expr * expr               (* e || e *)
    | And of loc * expr * expr              (* e && e *)
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

  and list_comp_clause =
    | DrawClause of pattern * expr (* p <- e *)
    | FilterClause of expr            (* e *)

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

  type header = {
      usage: string option
    ; description: string option
    ; options: flag_def list
    }

  let rec pretty_type = function
    | Forall (var, ty) -> "∀" ^ Name.pretty var ^ ". " ^ pretty_type ty
    | Var var -> Name.pretty var
    | Unif (u, name) -> Name.pretty name ^ "$" ^ Unique.display u
    | Number -> "Number"
    | Bool -> "Bool"
    | String -> "String"
    | Tuple tys -> "(" ^ String.concat ", " (Array.to_list (Array.map pretty_type tys)) ^ ")"
    | List ty -> "List(" ^ pretty_type ty ^ ")"

  let rec pretty_pattern = function
    | VarPat (_, x) -> Name.pretty x
    | ConsPat (_, x, xs) -> "(" ^ pretty_pattern x ^ ") : (" ^ pretty_pattern xs ^ ")"
    | ListPat (_, pats) -> "[" ^ String.concat ", " (List.map pretty_pattern pats) ^ "]"
    | TuplePat (_, pats) -> "(" ^ String.concat ", " (List.map pretty_pattern pats) ^ ")"
    | NumPat (_, f) -> Float.to_string f
    | OrPat (_, p1, p2) -> "(" ^ pretty_pattern p1 ^ " | " ^ pretty_pattern p2 ^ ")"

  let rec pretty = function
    | Var (_, x) -> Name.pretty x
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
    | MapLit (_, kvs) -> "{" ^ String.concat ", " (List.map (fun (k, e) -> k ^ ": " ^ pretty e) kvs) ^ "}"

    | MapLookup (_, expr, key) -> "(" ^ pretty expr ^ ")." ^ key
    | DynLookup (_, mexpr, kexpr) -> "(" ^ pretty mexpr ^ ")[" ^ pretty kexpr ^ "]"

    | Add (_, e1, e2)     -> "(" ^ pretty e1 ^ " + " ^ pretty e2 ^ ")"
    | Sub (_, e1, e2)     -> "(" ^ pretty e1 ^ " - " ^ pretty e2 ^ ")"
    | Mul (_, e1, e2)     -> "(" ^ pretty e1 ^ " * " ^ pretty e2 ^ ")"
    | Div (_, e1, e2)     -> "(" ^ pretty e1 ^ " / " ^ pretty e2 ^ ")"
    | Concat (_, e1, e2)  -> "(" ^ pretty e1 ^ " .. " ^ pretty e2 ^ ")"

    | Equals (_, e1, e2) -> "(" ^ pretty e1 ^ " == " ^ pretty e2 ^ ")"
    | NotEquals (_, e1, e2) -> "(" ^ pretty e1 ^ " != " ^ pretty e2 ^ ")"
    | LE (_, e1, e2)     -> "(" ^ pretty e1 ^ " <= " ^ pretty e2 ^ ")"
    | GE (_, e1, e2)     -> "(" ^ pretty e1 ^ " >= " ^ pretty e2 ^ ")"
    | LT (_, e1, e2)     -> "(" ^ pretty e1 ^ " <  " ^ pretty e2 ^ ")"
    | GT (_, e1, e2)     -> "(" ^ pretty e1 ^ " >  " ^ pretty e2 ^ ")"

    | Or (_, e1, e2)     -> "(" ^ pretty e1 ^ " || " ^ pretty e2 ^ ")"
    | And (_, e1, e2)    -> "(" ^ pretty e1 ^ " && " ^ pretty e2 ^ ")"
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
    | LetRecSeq (_, x, xs, e) -> "let rec " ^ Name.pretty x ^ "(" ^ String.concat ", " (List.map pretty_pattern xs) ^ ") = " ^ pretty e
    | LetEnvSeq (_, x, e) -> "let $" ^ x ^ " = " ^ pretty e
    | Let (_, x, e1, e2) ->
        "let " ^ pretty_pattern x ^ " = " ^ pretty e1 ^ " in " ^ pretty e2
    | LetRec (_, x, xs, e1, e2) -> "let rec " ^ Name.pretty x ^ "(" ^ String.concat ", " (List.map pretty_pattern xs) ^ ") = " ^ pretty e1 ^ " in " ^ pretty e2
    | LetEnv (_, x, e1, e2) -> "let $" ^ x ^ " = " ^ pretty e1 ^ " in " ^ pretty e2
    | Assign (_, x, e) -> Name.pretty x ^ " = " ^ pretty e
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

  let pretty_list (exprs : expr list) : string =
    List.fold_right (fun x r -> pretty x ^ "\n" ^ r) exprs ""

  let get_loc = function
    | Var (loc, _) | App (loc, _, _) | Lambda (loc, _, _) | StringLit (loc, _) | NumLit (loc, _)
    | BoolLit (loc, _) | UnitLit loc | NullLit loc | ListLit(loc, _) | TupleLit(loc, _) | MapLit(loc, _) 
    | MapLookup(loc, _, _) | DynLookup(loc, _, _) | Add(loc, _, _) | Sub(loc, _, _) | Mul(loc, _, _) 
    | Div(loc, _ , _) | Concat(loc, _, _) | Equals(loc, _, _) | NotEquals(loc, _, _) | LE(loc, _, _) 
    | GE(loc, _, _) | LT(loc, _, _) | GT(loc, _, _) | Or(loc, _, _) | And(loc, _, _) | Not(loc, _)
    | Range(loc, _, _) | ListComp(loc, _, _)
    | If(loc, _, _, _) | Seq(loc, _) | LetSeq(loc, _, _) | LetRecSeq(loc, _, _, _) | LetEnvSeq(loc, _, _) | Let(loc, _, _, _)
    | LetRec(loc, _, _, _, _) | LetEnv(loc, _, _, _) | Assign(loc, _, _) | ProgCall(loc, _, _) | Pipe(loc, _) | EnvVar(loc, _)
    | Async(loc, _) | Await(loc, _) | Match(loc, _, _)
    -> loc

  let get_pattern_loc = function
    | VarPat (loc, _) | ConsPat(loc, _, _) | ListPat (loc, _) | TuplePat (loc, _)
    | NumPat (loc, _) | OrPat (loc, _, _) 
    -> loc
end

type name = { name : string; index : int }

module Name = struct
  type t = name

  let original_name (name : t) = name.name

  let pretty (name : t) = name.name ^ "_" ^ string_of_int name.index

  (* Comparisons are purely based on the name index
     and therefore have no actual meaning.
  *)
  let compare (x : t) (y : t) : int = Int.compare x.index y.index
end

module Parsed = Make (struct
  type t = string

  let pretty (x : t) = x
end)

module Renamed = Make (Name)
