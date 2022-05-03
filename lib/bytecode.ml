open Ast

type op =
  (* locals *)
  | StoreLocal of int       (* stlor <local index> *)
  | LoadLocal of int        (* ldloc <local index> *)
  (* temporary registers *)
  | StoreTemp of int        (* sttemp <temp index> *)
  | LoadTemp of int         (* ldtemp <temp index> *)
  (* literals *)
  | FloatLit of float       (* float <literal> *)
  | StringLit of int        (* string <string index> *)
  | TrueLit                 (* true *)
  | FalseLit                (* false *)
  | UnitLit                 (* unit *)
  | ClosureLit of int       (* closure <function index> *)

  | AllocList of int        (* alloclist <argument count> *)
  (* stack manipulation *)
  | Dup                     (* dup *)
  (* functions *)
  | CallDyn                 (* calldyn *)
  | Call of int             (* call <function index> *)
  | CallPrim of int         (* callprim <prim index> *)
  (* program calls *)
  | CallProg of int * int   (* callprog <string index> <number of args> *)
  (* arithmetic *)
  | Add                     (* add *)
  | Sub                     (* sub *)
  | Mul                     (* mul *)
  | Div                     (* div *)
  (* control flow *)
  | Label of int            (* #<label> *)
  | Jump of int             (* jump <label> *)
  | JumpEQ of int           (* jumpeq <label> *)
  | JumpNEQ of int          (* jumpneq <label> *)
  | JumpLT of int           (* jumplt <label> *)
  | JumpGT of int           (* jumpgt <label> *)
  | JumpLE of int           (* jumple <label> *)
  | JumpGE of int           (* jumpge <label> *)

type function_block = {
    arg_count : int 
  ; body : op list
  }

type program = {
    functions : function_block list
  ; main : op list
  ; strings : string list
  }

let pretty_op = function
  | StoreLocal n -> "stloc " ^ string_of_int n
  | LoadLocal n -> "ldloc " ^ string_of_int n
  | StoreTemp n -> "sttemp " ^ string_of_int n
  | LoadTemp n -> "ldtemp " ^ string_of_int n
  | FloatLit f -> "float " ^ string_of_float f
  | StringLit i -> "string " ^ string_of_int i
  | TrueLit -> "true"
  | FalseLit -> "false"
  | UnitLit -> "unit"
  | ClosureLit i -> "closure " ^ string_of_int i
  | AllocList n -> "alloclist " ^ string_of_int n
  | Dup -> "dup"
  | CallDyn -> "calldyn"
  | Call i -> "call " ^ string_of_int i
  | CallPrim i -> "callprim " ^ string_of_int i
  | CallProg (ix, n) -> "callprog " ^ string_of_int ix ^ " " ^ string_of_int n
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Label ix -> "#" ^ string_of_int ix
  | Jump l -> "jump " ^ string_of_int l
  | JumpEQ l -> "jumpeq " ^ string_of_int l
  | JumpNEQ l -> "jumpneq " ^ string_of_int l
  | JumpLT l -> "jumplt " ^ string_of_int l
  | JumpGT l -> "jumpgt " ^ string_of_int l
  | JumpLE l -> "jumple " ^ string_of_int l
  | JumpGE l -> "jumpge " ^ string_of_int l

let pretty_function_block (index : int) (block : function_block) : string =
  string_of_int index ^ "(" ^ string_of_int block.arg_count ^ "):\n"
  ^ String.concat "\n" (List.map pretty_op block.body)

let pretty (program : program) : string =
    "<FUNCTIONS>\n" 
  ^ String.concat "\n\n" (List.mapi pretty_function_block program.functions) ^ "\n"
  ^ "<MAIN>\n"
  ^ String.concat "\n" (List.map pretty_op program.main) ^ "\n"
  ^ "<STRINGS>\n"
  (* Newlines in strings are not yet supported anyway, so we might as well handle 
     those in the bytecode interpreter and use newlines as seperators here *)
  ^ String.concat "\n" program.strings


