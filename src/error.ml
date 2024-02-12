open Syntax
open Errormessage
module Value = Eval.Value

type t =
  | Panic of string
  | TODO of string
  | LexError of Lexer.lex_error
  | ParseError of loc * string
  | SpecificParseError of Parserprelude.specific_parse_error
  | SysError of string
  | RenameError of Rename.rename_error
  | TypeErrors of (loc * Types.type_error) list
  | EvalError of Eval.eval_error

exception ReraisedTypeErrors of (loc * Types.type_error) list

let as_exn = function
  | Ok x -> x
  | Error err -> (
      match err with
      | Panic msg -> raise (Util.Panic msg)
      | TODO msg -> raise (Util.TODO msg)
      | LexError err -> raise (Lexer.LexError err)
      | ParseError (loc, msg) -> raise (Parserprelude.ParseError (loc, msg))
      | SpecificParseError err -> raise (Parserprelude.SpecificParseError err)
      | SysError msg -> raise (Sys_error msg)
      | RenameError err -> raise (Rename.RenameError err)
      | TypeErrors errors -> raise (ReraisedTypeErrors errors)
      | EvalError err -> raise (Eval.EvalError err))

let handle_errors : 'a. (t -> 'a) -> (unit -> 'a) -> 'a =
 fun handler thunk ->
  try thunk () with
  | Util.Panic msg -> handler (Panic msg)
  | Failure msg -> handler (Panic ("Failure: " ^ msg))
  | Util.TODO loc -> handler (TODO loc)
  | Lexer.LexError err -> handler (LexError err)
  | Parserprelude.ParseError (loc, msg) -> handler (ParseError (loc, msg))
  | Parserprelude.SpecificParseError err -> handler (SpecificParseError err)
  | Sys_error msg -> handler (SysError msg)
  | Rename.RenameError err -> handler (RenameError err)
  | ReraisedTypeErrors errors -> handler (TypeErrors errors)
  | Eval.EvalError err -> handler (EvalError err)
  | Assert_failure _ as exn ->
      handler (Panic ("Assertion failure: " ^ Printexc.to_string exn))
  | err -> handler (Panic ("Exception: " ^ Printexc.to_string err))

let pretty_call_trace (locs : loc list) =
  match locs with
  | [] -> ""
  | _ ->
      "Call trace:" ^ "\n    "
      ^ String.concat "\n    " (List.map Loc.pretty_start locs)

let pretty_reraised (locations : loc list) =
  match locations with
  | [] -> ""
  | _ ->
      "\nReraised at:"
      (* Reraise locations are accumulated in reverse so we need to use rev_map here *)
      ^ "\n    "
      ^ String.concat "\n    " (List.rev_map Loc.pretty_start locations)

let pretty_error : text_style -> (loc option -> string -> unit) -> t -> unit =
 fun text_style print_fun ->
  let pretty_unify_context pretty_type (original_type1, original_type2) =
    "\n    While trying to unify "
    ^ text_style.ty_secondary (pretty_type original_type1)
    ^ "\n" ^ "                      and "
    ^ text_style.ty_secondary (pretty_type original_type2)
  in
  let pretty_optional_unify_context pretty_type = function
    | None -> ""
    | Some context -> pretty_unify_context pretty_type context
  in
  function
  | Panic msg ->
      print_fun None
        ("PANIC! The 'impossible' happened (This is a bug in the Polaris \
          interpreter, please report it!):\n" ^ msg)
  | TODO loc ->
      print_fun None
        ("PANIC! Unresolved compiler TODO at '" ^ loc
       ^ "'.\n\
          If you see this, please report it and tell the author to finish \
          their things before releasing them!")
  | ParseError (loc, msg) -> print_fun (Some loc) ("Parse Error: " ^ msg)
  | SpecificParseError (MismatchedLetName (loc, name1, name2)) ->
      print_fun (Some loc)
        ("Function declared with different names.\n"
       ^ "    The type signature calls it        "
        ^ text_style.identifier name1
        ^ "\n" ^ "    but its definition refers to it as "
        ^ text_style.identifier name2
        ^ "")
  | SysError msg -> print_fun None ("System error: " ^ msg)
  | RenameError error -> begin
      match error with
      | VarNotFound (x, loc) ->
          print_fun (Some loc) ("Variable not found: " ^ text_style.identifier x)
      | ModuleVarNotFound (x, loc) ->
          print_fun (Some loc) ("Module not found: " ^ text_style.identifier x)
      | TyVarNotFound (x, loc) ->
          print_fun (Some loc) ("Type variable not found: " ^ text_style.ty x)
      | TyConNotFound (x, loc) ->
          print_fun (Some loc) ("Type constructor not found: " ^ text_style.ty x)
      | DataConNotFound (x, loc) ->
          print_fun (Some loc) ("Data constructor not found: " ^ text_style.ty x)
      | TooManyArgsToDataConPattern (name, patterns, loc) ->
          print_fun (Some loc)
            ("Too many arguments to data constructor "
            ^ text_style.identifier (Name.pretty name)
            ^ " in a pattern\n" ^ "    Data constructors always take exactly "
            ^ text_style.number 1 ^ " argument\n"
            ^ "                   but this one was given "
            ^ text_style.number (List.length patterns))
      | SubscriptVarNotFound (x, loc) ->
          print_fun (Some loc)
            ("Variable or module not found: " ^ text_style.identifier x)
      | LetSeqInNonSeq (expr, loc) ->
          print_fun (Some loc)
            ("Let expression without 'in' found outside a sequence expression.\n"
           ^ "    Expression: " ^ Parsed.pretty expr)
      | SubModuleNotFound (name, loc) ->
          print_fun (Some loc)
            ("Module does not contain a submodule named "
           ^ text_style.identifier name)
      | WrongNumberOfTyConArgs (name, expected_arg_count, args, loc) ->
          print_fun (Some loc)
            ("Invalid number of arguments supplied to type constructor "
            ^ text_style.ty (Name.pretty name)
            ^ ".\n" ^ "    The type constructor takes "
            ^ text_style.number expected_arg_count
            ^ " arguments\n" ^ "                 but was given "
            ^ text_style.number (List.length args))
      | NonExceptionInTry (name, loc) ->
          print_fun (Some loc)
            ("Invalid non-exception data constructor "
            ^ text_style.identifier (Name.pretty name)
            ^ " in an exception handler")
      | UnboundExportConstructor (name, loc) ->
          print_fun (Some loc)
            ("Exported constructor not found: " ^ text_style.identifier name)
      | DuplicateKeyInRecordUpdate (key, loc) ->
          print_fun (Some loc)
            ("Duplicate key in record update: " ^ text_style.identifier key)
      | NonClassInConstraint (class_constraint_type, loc) ->
          print_fun (Some loc)
            ("Non class type in constraint: "
            ^ text_style.ty (Renamed.pretty_type class_constraint_type))
      | NonClassInInstance (name, loc) ->
          print_fun (Some loc)
            ("Non class type in instance declaration: "
            ^ text_style.identifier (Name.pretty name))
      | WrongNumberOfClassArgs { class_name; expected; actual; loc } ->
          print_fun (Some loc)
            ("Invalid number of arguments to type class "
            ^ text_style.ty (Name.pretty class_name)
            ^ "\n" ^ "    The type class expects " ^ text_style.number expected
            ^ " arguments\n" ^ "             but was given "
            ^ text_style.number actual)
      | ClassMethodMismatch { class_name; missing; invalid; loc } -> (
          match (missing, invalid) with
          | _, [] ->
              print_fun (Some loc)
                ("An instance of the type class "
                ^ text_style.ty (Name.pretty class_name)
                ^ " is missing required methods:\n"
                ^ String.concat "\n"
                    (List.map
                       (fun method_name ->
                         "    - " ^ text_style.identifier method_name)
                       missing))
          | [], _ ->
              print_fun (Some loc)
                ("Invalid methods in an instance of the type class "
                ^ text_style.ty (Name.pretty class_name)
                ^ ":\n"
                ^ String.concat "\n"
                    (List.map
                       (fun method_name ->
                         "    - " ^ text_style.identifier method_name)
                       invalid))
          | _ ->
              print_fun (Some loc)
                ("An instance of the type class "
                ^ text_style.ty (Name.pretty class_name)
                ^ " is missing required methods:\n"
                ^ String.concat "\n"
                    (List.map
                       (fun method_name ->
                         "    - " ^ text_style.identifier method_name)
                       missing)
                ^ "\n    but also contains invalid methods:\n"
                ^ String.concat "\n"
                    (List.map
                       (fun method_name ->
                         "    - " ^ text_style.identifier method_name)
                       invalid)
                ^ "\n    Did you misspell something?"))
    end
  | EvalError error -> begin
      match error with
      | PolarisException (name, arguments, trace, message_lazy) ->
          let loc, locs, reraised =
            match trace with
            | NotYetRaised ->
                Util.panic __LOC__
                  "Global handler caught ostensibly unraised exception"
            | RaisedPreviously { original_trace = loc :: locs; reraised } ->
                (loc, locs, reraised)
            | RaisedPreviously _ ->
                Util.panic __LOC__
                  "Polaris exception did not carry original trace locations"
          in
          print_fun (Some loc)
            (text_style.error "Exception"
            ^ ": " ^ Lazy.force message_lazy ^ "\n    Caused by "
            ^ text_style.identifier (Name.pretty name)
            ^ "\n" ^ pretty_call_trace locs ^ pretty_reraised reraised)
      | PrimOpArgumentError (primop_name, vals, msg, loc :: locs) ->
          print_fun (Some loc)
            ("Invalid arguments to builtin function '" ^ primop_name ^ "': "
           ^ msg ^ "\n" ^ "    Arguments: " ^ Value.pretty (ListV vals) ^ "\n"
           ^ pretty_call_trace locs)
      | RuntimeError (msg, loc :: locs) ->
          print_fun (Some loc)
            ("Runtime error: " ^ msg ^ "\n" ^ pretty_call_trace locs)
      | NonExhaustiveMatch (value, loc :: locs) ->
          print_fun (Some loc)
            ("PANIC! (this is a bug, please report it)\n\
              Non-exhaustive pattern match does not cover value: "
           ^ Value.pretty value)
      | ArgParseError msg -> print_fun None msg
      | EnsureFailed (path, loc :: locs) ->
          print_fun (Some loc)
            ("Required command not installed: " ^ text_style.identifier path
           ^ "\n" ^ pretty_call_trace locs)
      | _ -> Util.panic __LOC__ "Invalid eval error"
    end
  | LexError err -> begin
      match err with
      | InvalidOperator (loc, name) ->
          print_fun (Some loc)
            ("Invalid operator: " ^ text_style.identifier name)
      | UnterminatedString -> print_fun None "Unterminated string"
      | InvalidChar (loc, char) ->
          print_fun (Some loc)
            ("Unexpected character "
            ^ text_style.identifier (Base.String.of_char char))
      | InvalidStringEscape (loc, str) ->
          print_fun (Some loc)
            ("Invalid string escape code: " ^ text_style.emphasis ("\\" ^ str))
    end
  | TypeErrors errors ->
      let pretty_type_error (loc, (err : Types.type_error)) =
        print_fun (Some loc)
          begin
            match err with
            | UnableToUnify ((ty1, ty2), unify_context) ->
                let pretty_type =
                  Disambiguate.builder |> Disambiguate.ty ty1
                  |> Disambiguate.ty ty2
                  |> Disambiguate.unify_context_option unify_context
                  |> Disambiguate.pretty_type
                in
                "Unable to unify types "
                ^ text_style.ty (pretty_type ty1)
                ^ "\n" ^ "                  and "
                ^ text_style.ty (pretty_type ty2)
                ^ pretty_optional_unify_context pretty_type unify_context
            | DifferentVariantConstrArgs
                (constructor_name, types1, types2, unify_context) ->
                let pretty_type =
                  Disambiguate.builder |> Disambiguate.types types1
                  |> Disambiguate.types types2
                  |> Disambiguate.unify_context unify_context
                  |> Disambiguate.pretty_type
                in
                "Unable to unify an instance of the variant constructor "
                ^ text_style.identifier constructor_name
                ^ "\n" ^ "                                             with "
                ^ text_style.number (List.length types1)
                ^ " fields\n"
                ^ "    with an instance of the same constructor with "
                ^ text_style.number (List.length types2)
                ^ " fields\n" ^ "    Specifically: Unable to match\n"
                ^ "        argument types ("
                ^ String.concat ", "
                    (List.map (fun ty -> text_style.ty (pretty_type ty)) types1)
                ^ ")\n" ^ "                  with ("
                ^ String.concat ", "
                    (List.map (fun ty -> text_style.ty (pretty_type ty)) types2)
                ^ ")"
                ^ pretty_unify_context pretty_type unify_context
            | MismatchedTyCon (constr_name1, constr_name2, unify_context) ->
                let pretty_type =
                  Disambiguate.builder
                  |> Disambiguate.unify_context_option unify_context
                  |> Disambiguate.pretty_type
                in
                "Unable to match data constructors "
                ^ text_style.ty (Name.pretty constr_name1)
                ^ " and "
                ^ text_style.ty (Name.pretty constr_name2)
                ^ pretty_optional_unify_context pretty_type unify_context
            | Impredicative ((ty1, ty2), unify_context) ->
                let pretty_type =
                  Disambiguate.builder |> Disambiguate.ty ty1
                  |> Disambiguate.ty ty2
                  |> Disambiguate.unify_context_option unify_context
                  |> Disambiguate.pretty_type
                in
                "Impredicative instantiation attempted\n"
                ^ "    when matching types "
                ^ text_style.ty (pretty_type ty1)
                ^ "\n" ^ "                    and "
                ^ text_style.ty (pretty_type ty2)
                ^ pretty_optional_unify_context pretty_type unify_context
                ^ "\n\
                   Unification involving forall-types is not supported (and \
                   most likely a bug)"
            | OccursCheck (typeref, name, ty, unify_context) ->
                let pretty_type =
                  Disambiguate.builder
                  |> Disambiguate.ty (Unif (typeref, name))
                  |> Disambiguate.ty ty
                  |> Disambiguate.unify_context_option unify_context
                  |> Disambiguate.pretty_type
                in
                "Unable to construct the infinite type "
                ^ text_style.ty (pretty_type (Unif (typeref, name)))
                ^ "\n" ^ "                                    ~ "
                ^ text_style.ty (pretty_type ty)
                ^ pretty_optional_unify_context pretty_type unify_context
            | FunctionsWithDifferentArgCounts (tys1, tys2, unify_context) ->
                let pretty_type =
                  Disambiguate.builder |> Disambiguate.types tys1
                  |> Disambiguate.types tys2
                  |> Disambiguate.unify_context unify_context
                  |> Disambiguate.pretty_type
                in
                "Unable to match a function type with "
                ^ text_style.number (List.length tys1)
                ^ " arguments with one that takes "
                ^ text_style.number (List.length tys2)
                ^ " arguments.\n" ^ "Unable to unify argument types "
                ^ String.concat ", "
                    (List.map (fun ty -> text_style.ty (pretty_type ty)) tys1)
                ^ "\n" ^ "                           and "
                ^ String.concat ", "
                    (List.map (fun ty -> text_style.ty (pretty_type ty)) tys2)
                ^ pretty_unify_context pretty_type unify_context
            | PassedIncorrectNumberOfArgsToFun
                (actual_count, expected_types, result_ty) ->
                let pretty_type =
                  Disambiguate.builder
                  |> Disambiguate.types expected_types
                  |> Disambiguate.ty result_ty |> Disambiguate.pretty_type
                in
                "Trying to pass "
                ^ text_style.number actual_count
                ^ " arguments to a function that expects "
                ^ text_style.number (List.length expected_types)
                ^ ".\n"
                ^ "Incorrect number of arguments passed to a function of type "
                ^ text_style.ty (pretty_type (Fun (expected_types, result_ty)))
            | IncorrectNumberOfArgsInLambda
                (actual_count, expected_types, result_ty) ->
                let pretty_type =
                  Disambiguate.builder
                  |> Disambiguate.types expected_types
                  |> Disambiguate.ty result_ty |> Disambiguate.pretty_type
                in
                "Incorrect number of parameters in lambda. This lambda takes "
                ^ text_style.number actual_count
                ^ " arguments\n"
                ^ "                  but its type suggests that it should take "
                ^ text_style.number (List.length expected_types)
                ^ ".\n" ^ "    When checking a lambda of expected type "
                ^ text_style.ty (pretty_type (Fun (expected_types, result_ty)))
            | MissingRecordFields (remaining1, remaining2, unify_context) ->
                let pretty_type =
                  Disambiguate.builder
                  |> Disambiguate.types (List.map snd remaining1)
                  |> Disambiguate.types (List.map snd remaining2)
                  |> Disambiguate.unify_context unify_context
                  |> Disambiguate.pretty_type
                in
                "Mismatched record fields.\n" ^ "Missing mutual record fields "
                ^ text_style.ty
                    (pretty_type (RecordClosed (Array.of_list remaining2)))
                ^ "\n" ^ "                         and "
                ^ text_style.ty
                    (pretty_type (RecordClosed (Array.of_list remaining1)))
                ^ "\n" ^ "                         respectively."
                ^ pretty_unify_context pretty_type unify_context
            | MissingVariantConstructors (remaining1, remaining2, unify_context)
              ->
                let pretty_type =
                  Disambiguate.builder
                  |> Disambiguate.types (List.concat_map snd remaining1)
                  |> Disambiguate.types (List.concat_map snd remaining2)
                  |> Disambiguate.unify_context unify_context
                  |> Disambiguate.pretty_type
                in
                "Mismatched variant constructors.\n"
                ^ "Missing mutual variant constructors "
                ^ text_style.ty
                    (pretty_type (VariantClosed (Array.of_list remaining2)))
                ^ "\n" ^ "                                and "
                ^ text_style.ty
                    (pretty_type (VariantClosed (Array.of_list remaining1)))
                ^ "\n" ^ "                                respectively."
                ^ pretty_unify_context pretty_type unify_context
            | ArgCountMismatchInDefinition (fun_name, types, count) ->
                "The function "
                ^ text_style.identifier (Name.pretty fun_name)
                ^ " is declared with " ^ text_style.number count
                ^ " parameters\n"
                ^ "    but it's type suggests that it should have "
                ^ text_style.number (List.length types)
            | NonFunTypeInLetRec (fun_name, ty) ->
                let pretty_type =
                  Disambiguate.builder |> Disambiguate.ty ty
                  |> Disambiguate.pretty_type
                in
                "The function definition for "
                ^ text_style.identifier (Name.pretty fun_name)
                ^ " is declared as a function\n"
                ^ "    but has a non-function type: "
                ^ text_style.ty (pretty_type ty)
            | CannotUnwrapNonData ty ->
                let pretty_type =
                  Disambiguate.builder |> Disambiguate.ty ty
                  |> Disambiguate.pretty_type
                in
                "Trying to unwrap invalid type "
                ^ text_style.ty (pretty_type ty)
                ^ "\n\
                  \    Unwrapping is only possible for types defined in a data \
                   declaration"
            | ValueRestriction ty ->
                let pretty_type =
                  Disambiguate.builder |> Disambiguate.ty ty
                  |> Disambiguate.pretty_type
                in
                "Value restriction violation\n" ^ "    Trying to bind "
                ^ text_style.emphasis "non-value"
                ^ " to a variable\n" ^ "    with a polymorphic type: "
                ^ text_style.ty (pretty_type ty)
            | SkolemUnifyEscape (unif, skol, ty, unify_context) ->
                let pretty_type =
                  Disambiguate.builder |> Disambiguate.ty unif
                  |> Disambiguate.ty skol |> Disambiguate.ty ty
                  |> Disambiguate.unify_context_option unify_context
                  |> Disambiguate.pretty_type
                in
                "Unable to match type "
                ^ text_style.ty (pretty_type unif)
                ^ " with a type involving the rigid type variable "
                ^ text_style.ty (pretty_type skol)
                ^ ".\n"
                ^ "    The rigid type variable would escape its scope.\n"
                ^ "    Unable to unify "
                ^ text_style.ty (pretty_type unif)
                ^ " and "
                ^ text_style.ty (pretty_type ty)
                ^ pretty_optional_unify_context pretty_type unify_context
            | DataConUnifyEscape (unif, constructor, ty, unify_context) ->
                let pretty_type =
                  Disambiguate.builder |> Disambiguate.ty unif
                  |> Disambiguate.ty ty
                  |> Disambiguate.unify_context_option unify_context
                  |> Disambiguate.pretty_type
                in
                "Unable to match type "
                ^ text_style.ty (pretty_type unif)
                ^ " with a type involving the type constructor "
                ^ text_style.identifier (Name.pretty constructor)
                ^ ".\n" ^ "    The type constructor "
                ^ text_style.identifier (Name.pretty constructor)
                ^ " would escape its scope.\n" ^ "    Unable to unify "
                ^ text_style.ty (pretty_type unif)
                ^ " and "
                ^ text_style.ty (pretty_type ty)
                ^ pretty_optional_unify_context pretty_type unify_context
            | IncorrectNumberOfExceptionArgs
                (name, given_arg_count, expected_types) ->
                "Incorrect number of arguments passed to exception constructor "
                ^ text_style.identifier (Name.pretty name)
                ^ ".\n" ^ "    This constructor expects "
                ^ text_style.number (List.length expected_types)
                ^ " arguments\n" ^ "               but was given "
                ^ text_style.number given_arg_count
            | PatternError pattern_error ->
                "Non-exhaustive pattern match\n"
                ^ begin
                    match pattern_error with
                    (* TODO: Think of something better to write here*)
                    | ListWithoutNil ->
                        "    Missing a pattern for " ^ text_style.emphasis "[]"
                    | ListWithoutCons ->
                        "    Missing a pattern for "
                        ^ text_style.emphasis "_ :: _"
                    | ExceptionWithoutWildcard ->
                        "    Match on "
                        ^ text_style.emphasis "exceptions"
                        ^ " is missing a wildcard case.\n"
                        ^ "    Pattern matching needs to handle every possible \
                           exception."
                    | NumWithoutWildcard ->
                        "    Match on "
                        ^ text_style.emphasis "numbers"
                        ^ " is missing a wildcard case.\n"
                        ^ "    Pattern matching needs to handle every possible \
                           number."
                    | StringWithoutWildcard ->
                        "    Match on "
                        ^ text_style.emphasis "strings"
                        ^ " is missing a wildcard case.\n"
                        ^ "    Pattern matching needs to handle every possible \
                           string."
                    | BoolWithout missing ->
                        "    Missing a pattern for "
                        ^ text_style.emphasis (string_of_bool missing)
                    | VariantNonExhaustive constructors ->
                        "    Unhandled constructors:\n"
                        ^ String.concat "\n"
                            (List.map (fun x -> "    - " ^ x) constructors)
                  end
            | MissingInstance { variables; class_name; arguments } ->
                let pretty_type =
                  Disambiguate.builder
                  |> Disambiguate.types arguments
                  |> Disambiguate.pretty_type
                in

                "Missing instance "
                ^ text_style.ty
                    (Name.pretty class_name ^ "("
                    ^ String.concat ", " (List.map pretty_type arguments)
                    ^ ")")
            | TupleLiteralOfWrongLength (expected, actual) ->
                let pretty_type =
                  Disambiguate.builder
                  |> Disambiguate.types (Array.to_list actual)
                  |> Disambiguate.pretty_type
                in
                "Tuple literal has " ^ text_style.number expected
                ^ " elements\n" ^ "But its type expects it to have "
                ^ text_style.number (Array.length actual)
                ^ ".\n" ^ "    Expected type: "
                ^ text_style.ty (pretty_type (Typed.Tuple actual))
            | NonProgramArgument ty ->
                let pretty_type =
                  Disambiguate.builder |> Disambiguate.ty ty
                  |> Disambiguate.pretty_type
                in
                "Invalid argument to program call\n"
                ^ "Program calls do not accept arguments of type "
                ^ text_style.ty (pretty_type ty)
            | NonInterpolatable ty ->
                let pretty_type =
                  Disambiguate.builder |> Disambiguate.ty ty
                  |> Disambiguate.pretty_type
                in
                "Non-interpolatable item in string interpolation\n"
                ^ "String interpolation does not accept arguments of type "
                ^ text_style.ty (pretty_type ty)
            | AmbiguousClassConstraint (class_constraint, matching_instances) ->
                let pretty_type =
                  Disambiguate.builder
                  |> Disambiguate.types class_constraint.arguments
                  |> Disambiguate.types (List.concat_map snd matching_instances)
                  |> Disambiguate.pretty_type
                in
                let pretty_instance variables arguments =
                  match variables with
                  | [] ->
                      text_style.ty
                        (Name.pretty class_constraint.class_name
                        ^ "("
                        ^ String.concat ", " (List.map pretty_type arguments)
                        ^ ")")
                  | variables ->
                      text_style.ty
                        ("forall "
                        ^ String.concat " " (List.map Name.pretty variables)
                        ^ ". "
                        ^ Name.pretty class_constraint.class_name
                        ^ "("
                        ^ String.concat ", " (List.map pretty_type arguments)
                        ^ ")")
                in

                "Ambiguous type class constraint "
                ^ text_style.ty
                    (Name.pretty class_constraint.class_name
                    ^ "("
                    ^ String.concat ", "
                        (List.map pretty_type class_constraint.arguments)
                    ^ ")")
                ^ "\n  Matching instances:" ^ "\n    - "
                ^ String.concat "\n    - "
                    (List.map
                       (fun (universals, arguments) ->
                         pretty_instance universals arguments)
                       matching_instances)
          end
      in
      List.iter pretty_type_error errors
