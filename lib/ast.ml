module Expr (Name : sig
  type t

  val pretty : t -> string
end) =
struct
  type name = Name.t

  type expr =
    (* Lambda calculus *)
    | Var of name                     (* x *)
    | App of expr * expr list         (* e (e₁, .., eₙ) *)
    | Lambda of name list * expr      (* \(x₁, .., xₙ) -> e*)
    (* Literals *)
    | StringLit of string             (* "str" *)
    | NumLit of float                 (* f *)
    | UnitLit                         (* () *)
    (* Common Operations *)
    | Add of expr * expr              (* e + e *)
    | Sub of expr * expr              (* e - e *)
    | Mul of expr * expr              (* e * e *)
    | Div of expr * expr              (* e / e *)
    | Equals of expr * expr           (* e == e *)
    | LE of expr * expr               (* e <= e *)
    | GE of expr * expr               (* e >= e *)
    | LT of expr * expr               (* e <  e *)
    | GT of expr * expr               (* e >  e *)
    (* Branching *)
    | If of expr * expr * expr        (* if e then e else e*)
    (* Sequencing *)
    | Seq of expr list                (* { e₁ ; .. ; eₙ } *)
    | LetSeq of name * expr           (* let x = e (Only valid inside `Seq` expressions) *)
    (* Mutable local definitions *)
    | Let of name * expr * expr       (* let x = e1 in e2 (valid everywhere) *)
    | Assign of name * expr           (* x = e *)
    (* Scripting capabilities *)
    | ProgCall of string * expr list  (* /p e₁ .. eₙ *)
    | Pipe of expr list               (* (e₁ | .. | eₙ) *)

  let rec pretty = function
    | Var x -> Name.pretty x
    | App (f, args) ->
        pretty f ^ "(" ^ String.concat ", " (List.map pretty args) ^ ")"
    | Lambda (params, e) ->
        "\\("
        ^ String.concat ", " (List.map Name.pretty params)
        ^ ") -> " ^ pretty e
    | StringLit l -> "\"" ^ l ^ "\""
    | NumLit f -> string_of_float f
    | UnitLit -> "()"

    | Add (e1, e2) -> "(" ^ pretty e1 ^ " + " ^ pretty e2 ^ ")"
    | Sub (e1, e2) -> "(" ^ pretty e1 ^ " - " ^ pretty e2 ^ ")"
    | Mul (e1, e2) -> "(" ^ pretty e1 ^ " * " ^ pretty e2 ^ ")"
    | Div (e1, e2) -> "(" ^ pretty e1 ^ " / " ^ pretty e2 ^ ")"

    | Equals (e1, e2) -> "(" ^ pretty e1 ^ " == " ^ pretty e2 ^ ")"
    | LE (e1, e2)     -> "(" ^ pretty e1 ^ " <= " ^ pretty e2 ^ ")"
    | GE (e1, e2)     -> "(" ^ pretty e1 ^ " >= " ^ pretty e2 ^ ")"
    | LT (e1, e2)     -> "(" ^ pretty e1 ^ " <  " ^ pretty e2 ^ ")"
    | GT (e1, e2)     -> "(" ^ pretty e1 ^ " >  " ^ pretty e2 ^ ")"

    | If (e1, e2, e3) -> "if " ^ pretty e1 ^ " then " ^ pretty e2 ^ " else " ^ pretty e3

    | Seq exprs -> "{ " ^ String.concat "; " (List.map pretty exprs) ^ "}"
    | LetSeq (x, e) -> "let " ^ Name.pretty x ^ " = " ^ pretty e
    | Let (x, e1, e2) ->
        "let " ^ Name.pretty x ^ " = " ^ pretty e1 ^ " in " ^ pretty e2
    | Assign (x, e) -> Name.pretty x ^ " = " ^ pretty e
    | ProgCall (prog, args) ->
        "/" ^ prog ^ " " ^ String.concat " " (List.map pretty args)
    | Pipe exprs -> String.concat " | " (List.map pretty exprs)

  let pretty_list (exprs : expr list) : string =
    List.fold_right (fun x r -> pretty x ^ "\n" ^ r) exprs ""
end

type name = { name : string; index : int }

module Name = struct
  type t = name

  let pretty (name : t) = name.name ^ "_" ^ string_of_int name.index

  (* Comparisons are purely based on the name index
     and therefore has no actual meaning.
  *)
  let compare (x : t) (y : t) : int = Int.compare x.index y.index
end

module StringExpr = Expr (struct
  type t = string

  let pretty (x : t) = x
end)

module NameExpr = Expr (Name)

type string_expr = StringExpr.expr
type name_expr = NameExpr.expr
