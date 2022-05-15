type loc = {
  file : string;
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int
}
module Loc = struct
  let pretty (loc : loc) =
    Printf.sprintf "%s:%d:%d-%d:%d" loc.file loc.start_line loc.start_col loc.end_line loc.end_col

  let from_pos (start_pos : Lexing.position) (end_pos : Lexing.position) : loc = {
      file=start_pos.pos_fname
    ; start_line = start_pos.pos_lnum
    (* pos_cnum is the offset between the beginning of the buffer and the position
      and pos_bol is the offset between the beginning of the buffer and the beginning of the current line
    *)
    ; start_col  = start_pos.pos_cnum - start_pos.pos_bol + 1
    ; end_line   = end_pos.pos_lnum
    ; end_col    = end_pos.pos_cnum - end_pos.pos_bol + 1
    }

end

module Expr (Name : sig
  type t

  val pretty : t -> string
end) =
struct
  type name = Name.t

  type expr =
    (* Lambda calculus *)
    | Var of loc * name                     (* x *)
    | App of loc * expr * expr list         (* e (e₁, .., eₙ) *)
    | Lambda of loc * name list * expr      (* \(x₁, .., xₙ) -> e*)
    (* Literals *)
    | StringLit of loc * string             (* "str" *)
    | NumLit of loc * float                 (* f *)
    | BoolLit of loc * bool                 (* true | false*)
    | UnitLit of loc                        (* () *)
    | NullLit of loc                        (* null *)
    | ListLit of loc * expr list            (* [e, .., e] *)
    | MapLit of loc * (string * expr) list  (* #{x₁: e₁, .., xₙ: eₙ}*)
    (* Common Operations *)
    | Add of loc * expr * expr              (* e + e *)
    | Sub of loc * expr * expr              (* e - e *)
    | Mul of loc * expr * expr              (* e * e *)
    | Div of loc * expr * expr              (* e / e *)
    | Concat of loc * expr * expr           (* e .. e*)
    | Equals of loc * expr * expr           (* e == e *)
    | LE of loc * expr * expr               (* e <= e *)
    | GE of loc * expr * expr               (* e >= e *)
    | LT of loc * expr * expr               (* e <  e *)
    | GT of loc * expr * expr               (* e >  e *)
    (* Branching *)
    | If of loc * expr * expr * expr        (* if e then e else e*)
    (* Sequencing *)
    | Seq of loc * expr list                      (* { e₁ ; .. ; eₙ } *)
    | LetSeq of loc * name * expr                 (* let x = e (Only valid inside `Seq` expressions) *)
    | LetRecSeq of loc * name * name list * expr  (* let rec f(x, .., x) = e*)
    (* Mutable local definitions *)
    | Let of loc * name * expr * expr                 (* let x = e1 in e2 (valid everywhere) *)
    | LetRec of loc * name * name list * expr * expr  (* let rec f(x, .., x) = e*)
    | Assign of loc * name * expr                     (* x = e *)
    (* Scripting capabilities *)
    | Print of loc * expr                   (* print(e) (Temporary. 'print' should really just be an intrinsic)*)
    | ProgCall of loc * string * expr list  (* /p e₁ .. eₙ *)
    | Pipe of loc * expr list               (* (e₁ | .. | eₙ) *)

  let rec pretty = function
    | Var (_, x) -> Name.pretty x
    | App (_, f, args) ->
        pretty f ^ "(" ^ String.concat ", " (List.map pretty args) ^ ")"
    | Lambda (_, params, e) ->
        "\\("
        ^ String.concat ", " (List.map Name.pretty params)
        ^ ") -> " ^ pretty e
    | StringLit (_,l) -> "\"" ^ l ^ "\""
    | NumLit (_, f) -> string_of_float f
    | BoolLit (_, b) -> string_of_bool b
    | UnitLit _ -> "()"
    | NullLit _ -> "null"
    | ListLit (_, exprs) -> "[" ^ String.concat ", " (List.map pretty exprs) ^ "]"
    | MapLit (_, kvs) -> "{" ^ String.concat ", " (List.map (fun (k, e) -> k ^ ": " ^ pretty e) kvs) ^ "}"

    | Add (_, e1, e2)     -> "(" ^ pretty e1 ^ " + " ^ pretty e2 ^ ")"
    | Sub (_, e1, e2)     -> "(" ^ pretty e1 ^ " - " ^ pretty e2 ^ ")"
    | Mul (_, e1, e2)     -> "(" ^ pretty e1 ^ " * " ^ pretty e2 ^ ")"
    | Div (_, e1, e2)     -> "(" ^ pretty e1 ^ " / " ^ pretty e2 ^ ")"
    | Concat (_, e1, e2)  -> "(" ^ pretty e1 ^ " .. " ^ pretty e2 ^ ")"

    | Equals (_, e1, e2) -> "(" ^ pretty e1 ^ " == " ^ pretty e2 ^ ")"
    | LE (_, e1, e2)     -> "(" ^ pretty e1 ^ " <= " ^ pretty e2 ^ ")"
    | GE (_, e1, e2)     -> "(" ^ pretty e1 ^ " >= " ^ pretty e2 ^ ")"
    | LT (_, e1, e2)     -> "(" ^ pretty e1 ^ " <  " ^ pretty e2 ^ ")"
    | GT (_, e1, e2)     -> "(" ^ pretty e1 ^ " >  " ^ pretty e2 ^ ")"

    | If (_, e1, e2, e3) -> "if " ^ pretty e1 ^ " then " ^ pretty e2 ^ " else " ^ pretty e3

    | Seq (_, exprs) -> "{ " ^ String.concat "; " (List.map pretty exprs) ^ "}"
    | LetSeq (_, x, e) -> "let " ^ Name.pretty x ^ " = " ^ pretty e
    | LetRecSeq (_, x, xs, e) -> "let rec " ^ Name.pretty x ^ "(" ^ String.concat ", " (List.map Name.pretty xs) ^ ") = " ^ pretty e
    | Let (_, x, e1, e2) ->
        "let " ^ Name.pretty x ^ " = " ^ pretty e1 ^ " in " ^ pretty e2
    | LetRec (_, x, xs, e1, e2) -> "let rec " ^ Name.pretty x ^ "(" ^ String.concat ", " (List.map Name.pretty xs) ^ ") = " ^ pretty e1 ^ " in " ^ pretty e2
    | Assign (_, x, e) -> Name.pretty x ^ " = " ^ pretty e
    | Print (_, e) -> "print(" ^ pretty e ^ ")"
    | ProgCall (_, prog, args) ->
        "!" ^ prog ^ " " ^ String.concat " " (List.map pretty args)
    | Pipe (_, exprs) -> String.concat " | " (List.map pretty exprs)

  let pretty_list (exprs : expr list) : string =
    List.fold_right (fun x r -> pretty x ^ "\n" ^ r) exprs ""

  let get_loc = function
    | Var (loc, _) | App (loc, _, _) | Lambda (loc, _, _) | StringLit (loc, _) | NumLit (loc, _)
    | BoolLit (loc, _) | UnitLit loc | NullLit loc | ListLit(loc, _) | MapLit(loc, _) | Add(loc, _, _) | Sub(loc, _, _) | Mul(loc, _, _) 
    | Div(loc, _ , _) | Concat(loc, _, _) | Equals(loc, _, _) | LE(loc, _, _) | GE(loc, _, _) | LT(loc, _, _) | GT(loc, _, _)
    | If(loc, _, _, _) | Seq(loc, _) | LetSeq(loc, _, _) | LetRecSeq(loc, _, _, _) | Let(loc, _, _, _)
    | LetRec(loc, _, _, _, _) | Assign(loc, _, _) | Print(loc, _) | ProgCall(loc, _, _) | Pipe(loc, _)
    -> loc
end

type name = { name : string; index : int }

module Name = struct
  type t = name

  let pretty (name : t) = name.name ^ "_" ^ string_of_int name.index

  (* Comparisons are purely based on the name index
     and therefore have no actual meaning.
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
