open Syntax
open Driver
open Errormessage
module Value = Eval.Value

type t = Panic of string
       | TODO of string
       | LexError of Lexer.lex_error
       | ParseError of loc * string
       | SpecificParseError of specific_parse_error 
       | SysError of string
       | RenameError of Rename.rename_error
       | TypeError of loc * Types.type_error
       | EvalError of Eval.eval_error

let handle_errors : (t -> 'a) -> (unit -> 'a) -> 'a =
  fun handler thunk ->
    try thunk () with
    | Util.Panic msg -> handler (Panic msg)
    | Util.TODO loc -> handler (TODO loc)
    | Lexer.LexError err -> handler (LexError err)
    | Driver.ParseError (loc, msg) -> handler (ParseError (loc, msg))
    | Driver.SpecificParseError err -> handler (SpecificParseError err)
    | Sys_error msg -> handler (SysError msg)
    | Rename.RenameError err -> handler (RenameError err)
    | Types.TypeError (loc, err) -> handler (TypeError (loc, err))
    | Eval.EvalError err -> handler (EvalError err)


let pretty_call_trace (locs : loc list) =
  match locs with
  | [] -> ""
  | _ -> "Call trace:"
        ^ "\n    " ^ String.concat "\n    " (List.map Loc.pretty locs)    

let pretty_error : text_style -> (loc option -> string -> 'a) -> t -> 'a = 
  fun text_style print_fun -> function
  | Panic msg -> print_fun None ("PANIC! The 'impossible' happened (This is a bug, please report it!):\n" ^ msg)
  | TODO loc -> print_fun None ("PANIC! Unresolved compiler TODO at '" ^ loc ^ "'.\nIf you see this, please report it and tell the author to finish their things before releasing them!")
  | ParseError (loc, msg) -> print_fun (Some loc) ("Parse Error: " ^ msg)
  | SpecificParseError (MismatchedLetName(loc, name1, name2)) ->
    print_fun (Some loc) ("Function declared with different names.\n"
            ^ "    The type signature calls it        " ^ text_style.identifier name1 ^ "\n"
            ^ "    but its definition refers to it as " ^ text_style.identifier name2 ^ "")
  | SysError msg -> print_fun None ("System error: " ^ msg)

  | RenameError error -> begin match error with
    | VarNotFound (x, loc) -> print_fun (Some loc) ("Variable not found: " ^ text_style.identifier x)
    | ModuleVarNotFound (x, loc) -> print_fun (Some loc) ("Module not found: " ^ text_style.identifier x)
    | TyVarNotFound (x, loc) -> print_fun (Some loc) ("Type variable or constructor not found: " ^ text_style.ty x)
    | TyConNotFound (x, loc) -> print_fun (Some loc) ("Type constructor not found: " ^ text_style.ty x)
    | SubscriptVarNotFound (x, loc) -> 
      print_fun (Some loc) ("Variable or module not found: " ^ text_style.identifier x)
    | LetSeqInNonSeq (expr, loc) -> print_fun (Some loc)
        ( "Let expression without 'in' found outside a sequence expression.\n"
        ^ "    Expression: " ^ Parsed.pretty expr)
        
    | SubModuleNotFound (name, loc) ->
      print_fun (Some loc) ("Module does not contain a submodule named " ^ text_style.identifier name)
    | HigherRankType (_ty, loc) ->
      print_fun (Some loc) ("Illegal inner forall in type.\n    Polaris does not support higher rank polymorphism")
    | WrongNumberOfTyConArgs(name, expected_arg_count, args, loc) ->
      print_fun (Some loc) ("Invalid number of arguments supplied to type constructor " ^ text_style.ty (Name.pretty name) ^ ".\n"
      ^ "    The type constructor takes " ^ text_style.number expected_arg_count ^ " arguments\n"
      ^ "                 but was given " ^ text_style.number (List.length args))
    end
  | EvalError error -> begin match error with
    | MapDoesNotContain (map, key, loc::locs) -> 
      print_fun (Some loc) ("Map does not contain key '" ^ key ^ "': " ^ Value.pretty (RecordV map)
        ^ "\n" ^ pretty_call_trace locs
      )
    | PrimOpArgumentError (primop_name, vals, msg, loc::locs) -> 
      print_fun (Some loc) ("Invalid arguments to builtin function '" ^ primop_name ^ "': " ^ msg ^ "\n"
                      ^ "    Arguments: " ^ Value.pretty (ListV vals)
                      ^ "\n" ^ pretty_call_trace locs
      )
    | RuntimeError (msg, loc::locs) -> 
      print_fun (Some loc) ("Runtime error: " ^ msg
      ^ "\n" ^ pretty_call_trace locs
    )
    | NonExhaustiveMatch (value, loc::locs) -> 
      print_fun (Some loc) ("Non-exhaustive pattern match does not cover value: " ^ Value.pretty value
    )
    | ArgParseError msg -> print_fun None msg

    | EnsureFailed (path, loc :: locs) -> 
      print_fun (Some loc) ("Required command not installed: " ^ text_style.identifier path
      ^ "\n" ^ pretty_call_trace locs
    )
    | _ -> Util.panic __LOC__ "Invalid eval error"
  end
  | LexError err -> begin match err with
    | InvalidOperator (loc, name) -> 
      print_fun (Some loc) ("Invalid operator: " ^ text_style.identifier name)
    | UnterminatedString -> print_fun None (
        "Unterminated string"
      )
    | InvalidChar (loc, char) -> print_fun (Some loc) ("Invalid char " ^ text_style.identifier (Base.String.of_char char))
  end
  | TypeError (loc, err) -> print_fun (Some loc) begin match err with
    | UnableToUnify ((ty1, ty2), (original_ty1, original_ty2)) -> 
                       "Unable to unify types " ^ text_style.ty (Renamed.pretty_type ty1) ^ "\n"
                     ^ "                  and " ^ text_style.ty (Renamed.pretty_type ty2) ^ "\n"
                     ^ "While trying to unify " ^ text_style.ty_secondary (Renamed.pretty_type original_ty1) ^ "\n"
                     ^ "                  and " ^ text_style.ty_secondary (Renamed.pretty_type original_ty2)
    | DifferentVariantConstrArgs (constructor_name, types1, types2, original_type1, original_type2) ->
                       "Unable to unify an instance of the variant constructor " ^ text_style.identifier constructor_name ^ "\n"
                     ^ "                                             with " ^ text_style.number (List.length types1) ^ " fields\n"
                     ^ "    with an instance of the same constructor with " ^ text_style.number (List.length types2) ^ " fields\n"
                     ^ "    Specifically: Unable to match\n"
                     ^ "        argument types (" ^ String.concat ", " (List.map (fun ty -> text_style.ty (Renamed.pretty_type ty)) types1) ^ ")\n"
                     ^ "                  with (" ^ String.concat ", " (List.map (fun ty -> text_style.ty (Renamed.pretty_type ty)) types2) ^ ")\n"
                     ^ "    While trying to unify " ^ text_style.ty_secondary (Renamed.pretty_type original_type1) ^ "\n"
                     ^ "                      and " ^ text_style.ty_secondary (Renamed.pretty_type original_type2)
    | MismatchedTyCon (constr_name1, constr_name2, original_ty1, original_ty2) ->
                       "Unable to match data constructors " ^ text_style.ty (Name.pretty constr_name1) ^ " and " ^ text_style.ty (Name.pretty constr_name2) ^ "\n"
                     ^ "While trying to unify " ^ text_style.ty_secondary (Renamed.pretty_type original_ty1) ^ "\n"
                     ^ "                  and " ^ text_style.ty_secondary (Renamed.pretty_type original_ty2)

    | Impredicative ((ty1, ty2), (original_ty1, original_ty2)) ->
        "Impredicative instantiation attempted\n"
      ^ "    when matching types " ^ text_style.ty (Renamed.pretty_type ty1) ^ "\n"
      ^ "                    and " ^ text_style.ty (Renamed.pretty_type ty2) ^ "\n"
      ^ "While trying to unify " ^ text_style.ty_secondary (Renamed.pretty_type original_ty1) ^ "\n"
      ^ "                  and " ^ text_style.ty_secondary (Renamed.pretty_type original_ty2) ^ "\n"
      ^ "Unification involving forall-types is not supported (and most likely a bug)"
    | OccursCheck (typeref, name, ty, original_ty1, original_ty2) -> 
                        "Unable to construct the infinite type " ^ text_style.ty (Renamed.pretty_type (Unif (typeref, name))) ^ "\n"
                      ^ "                                    ~ " ^ text_style.ty (Renamed.pretty_type ty) ^ "\n"
                     ^ "While trying to unify " ^ text_style.ty_secondary (Renamed.pretty_type original_ty1) ^ "\n"
                     ^ "                  and " ^ text_style.ty_secondary (Renamed.pretty_type original_ty2)
    | FunctionsWithDifferentArgCounts (tys1, tys2, original_ty1, original_ty2) ->
                        "Unable to match a function type with " ^ text_style.number (List.length tys1) ^ " arguments with one that takes " ^ text_style.number (List.length tys2) ^ " arguments.\n"
                      ^ "Unable to unify argument types " ^ String.concat ", " (List.map (fun ty -> text_style.ty (Renamed.pretty_type ty)) tys1) ^ "\n"
                      ^ "                           and " ^ String.concat ", " (List.map (fun ty -> text_style.ty (Renamed.pretty_type ty)) tys2) ^ "\n"
                      ^ "While trying to unify " ^ text_style.ty_secondary (Renamed.pretty_type original_ty1) ^ "\n"
                      ^ "                  and " ^ text_style.ty_secondary (Renamed.pretty_type original_ty2)
    | PassedIncorrectNumberOfArgsToFun (actual_count, expected_types, result_ty) ->
        "Trying to pass " ^ text_style.number actual_count ^ " arguments to a function that expects " ^ text_style.number (List.length expected_types) ^ ".\n"
      ^ "Incorrect number of arguments passed to a function of type " ^ text_style.ty (Renamed.pretty_type (Fun(expected_types, result_ty)))
    | IncorrectNumberOfArgsInLambda (actual_count, expected_types, result_ty) ->
        "Incorrect number of parameters in lambda. This lambda takes " ^ text_style.number actual_count ^ " arguments\n"
      ^ "                  but its type suggests that it should take " ^ text_style.number (List.length expected_types)  ^ ".\n"
      ^ "    When checking a lambda of expected type " ^ text_style.ty (Renamed.pretty_type (Fun(expected_types, result_ty)))
    | NonProgCallInPipe expr ->
      (* TODO: Is this even possible? *)
      "Non program call expression in a pipe."
    | MissingRecordFields (remaining1, remaining2, original_ty1, original_ty2) ->
                       "Mismatched record fields.\n"
                     ^ "Missing mutual record fields " ^ text_style.ty (Renamed.pretty_type (RecordClosed (Array.of_list remaining2))) ^ "\n"
                     ^ "                         and " ^ text_style.ty (Renamed.pretty_type (RecordClosed (Array.of_list remaining1))) ^ "\n"
                     ^ "                         respectively."
    | MissingVariantConstructors (remaining1, remaining2, original_ty1, original_ty2) ->
                      "Mismatched variant constructors.\n"
                    ^ "Missing mutual variant constructors " ^ text_style.ty (Renamed.pretty_type (VariantClosed (Array.of_list remaining2))) ^ "\n"
                    ^ "                                and " ^ text_style.ty (Renamed.pretty_type (VariantClosed (Array.of_list remaining1))) ^ "\n"
                    ^ "                                respectively."                
    | ArgCountMismatchInDefinition (fun_name, types, count) ->
                       "The function " ^ text_style.identifier (Name.pretty fun_name) ^ " is declared with " ^ text_style.number count ^ " parameters\n"
                     ^ "    but it's type suggests that it should have " ^ text_style.number (List.length types)
    | NonFunTypeInLetRec(fun_name, ty) ->
      "The function definition for " ^ text_style.identifier (Name.pretty fun_name) ^ " is declared as a function but has a non-function type."
    | CannotUnwrapNonData ty ->
      "Trying to unwrap invalid type " ^ text_style.ty (Renamed.pretty_type ty) ^ "\n    Unwrapping is only possible for types defined in a data declaration"
  end

