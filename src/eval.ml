open Syntax
open Syntax.Typed
open Util
module VarMap = Map.Make (Name)
module EnvMap = Trie.String
module RecordVImpl = Multimap.Make (String)

type eval_capabilities = {
  switch : Eio.Switch.t;
  fs : Eio.Fs.dir Eio.Path.t;
  mgr : Eio.Process.mgr;
}

type eval_env = {
  vars : value VarMap.t;
  env_vars : string EnvMap.t;
  argv : string list;
  call_trace : loc list;
  last_status : int ref;
  module_vars : runtime_module VarMap.t;
  exceptions : (name list * eval_env * expr) VarMap.t;
}

and runtime_module = {
  mod_vars : value VarMap.t;
  modules : runtime_module VarMap.t;
}

and value =
  | StringV of string
  | NumV of float
  (* The closure environment has to be lazy to
     support recursive lets, since the definition of a recursive function has
     to be stored in its own closure, which also stores its on environment, etc.*)
  | ClosureV of eval_env lazy_t * Typed.pattern list * Typed.expr
  (* PrimOps should be mostly indistinguishable from regular closures.
     The only exception is pretty printing, where primops are printed as
     "<primop: name>" instead of <closure(args)>
     Also, primop names are represented as strings, not names, since
     they are definitely unique and there is no point in keeping
     an additional index *)
  | PrimOpV of string
  | BoolV of bool
  (* Lists are *immutable*.
     Otherwise we would have to deal with some kind of a 'place' system, which is
     anything but ideal. One should be able to approximate mutable lists
     by mutable references to immutable lists 99% of the time, so this is
     hopefully not going to be an issue. *)
  | ListV of value list
  | TupleV of value array
  | RecordV of value RecordVImpl.t
  (* Represents a concurrent thread of execution *)
  | PromiseV of value Eio.Promise.t
  (* Until there are type classes, we need to keep the constructor name
     for pretty printing purposes *)
  | DataConV of name * value
  (* Data constructors can be used like functions *)
  | PartialDataConV of name
  | PartialExceptionV of name * name list * eval_env * expr
  | ExceptionV of name * (name * value) list * exception_trace * string Lazy.t
  | VariantConstructorV of string * value list
  | RefV of value ref

and exception_trace =
  | NotYetRaised
  | RaisedPreviously of {
      original_trace : loc list;
      reraised : loc list;
    }

let unitV = RecordV RecordVImpl.empty

let isUnitV = function
  | RecordV record ->
      RecordVImpl.equal (fun _ _ -> false) record RecordVImpl.empty
  | _ -> false

type eval_error =
  | PolarisException of
      name * (name * value) list * exception_trace * string Lazy.t
  | IndexOutOfRange of value list * int * loc list
  | RuntimeError of string * loc list
  | PrimOpArgumentError of string * value list * string * loc list
  | ArgParseError of string
  | NonExhaustiveMatch of value * loc list
  | EnsureFailed of string * loc list

exception EvalError of eval_error

module Value = struct
  type t = value

  let rec pretty (x : t) : string =
    match x with
    | StringV s -> "\"" ^ s ^ "\""
    | NumV n ->
        if Float.is_integer n then string_of_int (int_of_float n)
        else string_of_float n
    | ClosureV (_, params, _) ->
        "<closure("
        ^ String.concat ", " (List.map Syntax.Typed.pretty_pattern params)
        ^ ")>"
    | PrimOpV name -> "<primop: " ^ name ^ ">"
    | BoolV b -> string_of_bool b
    | ListV vals -> "[" ^ String.concat ", " (List.map pretty vals) ^ "]"
    | TupleV vals ->
        "(" ^ String.concat ", " (Array.to_list (Array.map pretty vals)) ^ ")"
    | RecordV kvs ->
        let kv_list = List.of_seq (RecordVImpl.to_seq kvs) in
        "{ "
        ^ String.concat ", "
            (List.map (fun (k, v) -> k ^ " = " ^ pretty v) kv_list)
        ^ " }"
    | PromiseV p -> begin
        match Eio.Promise.peek p with
        | Some value -> "<promise: " ^ pretty value ^ ">"
        | None -> "<promise: <<pending>>>"
      end
    | DataConV (constructor_name, value) ->
        constructor_name.name ^ "(" ^ pretty value ^ ")"
    | PartialDataConV constructor_name ->
        "<constructor: " ^ Name.pretty constructor_name ^ ">"
    | VariantConstructorV (constructor_name, []) -> "`" ^ constructor_name
    | VariantConstructorV (constructor_name, args) ->
        "`" ^ constructor_name ^ "("
        ^ String.concat ", " (List.map pretty args)
        ^ ")"
    | PartialExceptionV (name, parameter_names, _message_env, _message_expr) ->
        "<exception constructor: " ^ Name.pretty name ^ "("
        ^ String.concat ", " (List.map Name.pretty parameter_names)
        ^ ")>"
    | ExceptionV (exception_name, args, _call_trace, _message_closure) ->
        Name.pretty exception_name ^ "("
        ^ String.concat ", "
            (List.map
               (fun (name, value) -> Name.pretty name ^ " = " ^ pretty value)
               args)
        ^ ")"
    | RefV value -> "<ref: " ^ pretty !value ^ ">"

  let rec as_args (fail : t -> 'a) (x : t) : string list =
    match x with
    | StringV v -> [ v ]
    | NumV _
    | BoolV _ ->
        [ pretty x ]
    (* TODO: Should records/maps be converted to JSON? *)
    | ClosureV _
    | PrimOpV _
    | TupleV _
    | RecordV _
    | PromiseV _
    | DataConV _
    | PartialDataConV _
    | RefV _
    | VariantConstructorV _
    | PartialExceptionV _
    | ExceptionV _ ->
        fail x
    | ListV x -> List.concat_map (as_args fail) x
end

let lookup_var (env : eval_env) (loc : loc) (var : name) : value =
  try VarMap.find var env.vars with
  | Not_found ->
      panic __LOC__
        (Loc.pretty loc ^ ": Variable not found at runtime: " ^ Name.pretty var)

let insert_var : name -> value -> eval_env -> eval_env =
 fun var value env -> { env with vars = VarMap.add var value env.vars }

let insert_env_var : loc -> string -> value -> eval_env -> eval_env =
 fun loc var value env ->
  match value with
  | StringV x -> { env with env_vars = EnvMap.add var x env.env_vars }
  | (NumV _ | BoolV _) as v ->
      { env with env_vars = EnvMap.add var (Value.pretty v) env.env_vars }
  | ClosureV _
  | ListV _
  | TupleV _
  | PrimOpV _
  | RecordV _
  | PromiseV _
  | DataConV _
  | PartialDataConV _
  | VariantConstructorV _
  | RefV _
  | PartialExceptionV _
  | ExceptionV _ ->
      panic __LOC__
        (Loc.pretty loc ^ ": Invalid env var value: " ^ Value.pretty value)

let insert_module_var : name -> runtime_module -> eval_env -> eval_env =
 fun var module_value env ->
  { env with module_vars = VarMap.add var module_value env.module_vars }

let full_env_vars : eval_env -> string array =
 fun env ->
  Array.append (Unix.environment ())
    (Array.of_seq
       (Seq.map (fun (x, y) -> x ^ "=" ^ y) (EnvMap.to_seq env.env_vars)))

let rec val_eq (x : value) (y : value) : bool =
  match (x, y) with
  | StringV x, StringV y -> String.compare x y = 0
  | NumV x, NumV y -> Float.equal x y
  (* Closure comparison always evaluates to false.
      We're not going to solve the halting problem for this. *)
  | ClosureV _, _
  | _, ClosureV _ ->
      false
  | BoolV x, BoolV y -> x = y
  (* Lists are compared elements-wise, *not* by reference
     (whatever that would even mean with polaris' (more or less)
     referentially transparent semantics) *)
  | ListV xs, ListV ys ->
      if List.compare_lengths xs ys != 0 then false
      else List.for_all2 val_eq xs ys
  | RecordV m1, RecordV m2 -> RecordVImpl.equal val_eq m1 m2
  (* Comparisons of different values are always false *)
  | _, _ -> false

let trim_output = function
  | "" -> ""
  | str -> (
      match str.[String.length str - 1] with
      | '\n' -> String.sub str 0 (String.length str - 1)
      | _ -> str)

let handle_process_exceptions loc env cont =
  try[@warning "-52"] cont () with
  (* This needs to match on the failure message until eio
     raises something more robust *)
  | Failure "execve: Argument list too long" ->
      let trace =
        RaisedPreviously
          { original_trace = loc :: env.call_trace; reraised = [] }
      in
      raise_notrace
        (EvalError
           (PolarisException
              ( Primops.program_args_too_large_exception,
                [],
                trace,
                lazy
                  ("Arguments to program call are to large.\n" ^ "    "
                 ^ "Note: This is about the total size, not the number of \
                    arguments") )))
  | Eio.Io (Eio.Process.(E (Executable_not_found executable)), _) ->
      let trace =
        RaisedPreviously
          { original_trace = loc :: env.call_trace; reraised = [] }
      in
      raise_notrace
        (EvalError
           (PolarisException
              ( Primops.executable_not_found_exception,
                [
                  ( Name.{ name = "executable"; index = primop_index },
                    StringV executable );
                ],
                trace,
                lazy ("Executable not found: " ^ executable) )))

let rec match_pat_opt (pat : Typed.pattern) (scrut : value) :
    (eval_env -> eval_env) option =
  let ( let* ) = Option.bind in
  match (pat, scrut) with
  (* Type patterns are ignored at runtime. These have already been
     checked by the typechecker *)
  | TypePat (_, pat, _), scrut -> match_pat_opt pat scrut
  | VarPat (_, x), scrut -> Some (fun env -> insert_var x scrut env)
  | AsPat (_, pattern, name), scrut ->
      let opt_env_trans = match_pat_opt pattern scrut in
      Option.map
        (fun env_trans -> insert_var name scrut << env_trans)
        opt_env_trans
  | ConsPat (_, p, ps), ListV (v :: vs) ->
      let* x_trans = match_pat_opt p v in
      let* xs_trans = match_pat_opt ps (ListV vs) in
      Some (fun env -> xs_trans (x_trans env))
  | ListPat (_, ps), ListV vs ->
      if List.compare_lengths ps vs != 0 then None
      else
        let* transformations =
          Util.sequence_options (List.map2 match_pat_opt ps vs)
        in
        Some (Util.compose transformations)
  | TuplePat (_, ps), TupleV vs ->
      let pats = Array.of_list ps in
      (* This check is technically redundant once the type checker is implemented *)
      if Array.length pats != Array.length vs then None
      else
        let* transformations =
          Util.sequence_options_array (Array.map2 match_pat_opt pats vs)
        in
        Some
          (Array.fold_right
             (fun t r env -> t (r env))
             transformations
             (fun x -> x))
  | RecordPat (loc, field_patterns, extension_pattern), RecordV recordMap ->
      let exception MatchFailure in
      begin
        try
          let remaining_fields, env_transformers =
            Array.fold_left_map
              (fun fields (field_name, pattern) ->
                let value =
                  match RecordVImpl.find_first field_name fields with
                  | None ->
                      panic __LOC__
                        (Loc.pretty loc
                       ^ ": Record field not found at runtime: '" ^ field_name
                       ^ "'")
                  | Some value -> value
                in

                let env_trans =
                  match match_pat_opt pattern value with
                  | Some env_trans -> env_trans
                  | None -> raise MatchFailure
                in

                let remaining_fields =
                  RecordVImpl.delete_first field_name fields
                in

                (remaining_fields, env_trans))
              recordMap field_patterns
          in
          let* extension_trans =
            match extension_pattern with
            | None -> Some Fun.id
            | Some pattern -> match_pat_opt pattern (RecordV remaining_fields)
          in
          Some (extension_trans << Util.compose_array env_transformers)
        with
        | MatchFailure -> None
      end
  | NumPat (_, f1), NumV f2 when Float.equal f1 f2 -> Some (fun x -> x)
  | StringPat (_, literal), StringV str when String.equal literal str ->
      Some Fun.id
  | BoolPat (_, expected), BoolV value when Bool.equal expected value ->
      Some Fun.id
  | OrPat (_, p1, p2), v -> begin
      match match_pat_opt p1 v with
      | Some t -> Some t
      | None -> match_pat_opt p2 v
    end
  | ( DataPat (_, constructor_name, pattern),
      DataConV (val_constructor_name, underlying) )
    when constructor_name = val_constructor_name ->
      match_pat_opt pattern underlying
  | ( VariantPat (_, constructor_name, patterns),
      VariantConstructorV (val_constructor_name, values) )
    when String.equal constructor_name val_constructor_name ->
      let* transformations =
        Util.sequence_options (List.map2 match_pat_opt patterns values)
      in
      Some (Util.compose transformations)
  | ( ExceptionDataPat (_, name, patterns),
      ExceptionV (exception_name, args, trace, lazy_message) )
    when Name.equal name exception_name ->
      let* transformations =
        Util.sequence_options
          (List.map2
             (fun pattern (_, arg) -> match_pat_opt pattern arg)
             patterns args)
      in
      Some (Util.compose transformations)
  | _ -> None

let match_pat pat scrut locs =
  match match_pat_opt pat scrut with
  | None -> raise (EvalError (NonExhaustiveMatch (scrut, locs)))
  | Some trans -> trans

let rec match_params patterns arg_vals locs =
  match (patterns, arg_vals) with
  | [], [] -> fun x -> x
  | [], _
  | _, [] ->
      panic __LOC__ "Invalid number of arguments at runtime"
  | pat :: pats, arg :: args ->
      let trans = match_pat pat arg locs in
      fun s -> match_params pats args locs (trans s)

let raise_command_failure_exception env loc program program_args exit_code
    stdout =
  let trace =
    RaisedPreviously { original_trace = loc :: env.call_trace; reraised = [] }
  in
  raise_notrace
    (EvalError
       (PolarisException
          ( Primops.command_failure_exception,
            [
              ( { name = "result"; index = Name.primop_index },
                RecordV
                  (RecordVImpl.of_list
                     [
                       ("program", StringV program);
                       ( "arguments",
                         ListV (List.map (fun x -> StringV x) program_args) );
                       ("exitCode", NumV (float_of_int exit_code));
                       ("stdout", StringV stdout);
                     ]) );
            ],
            trace,
            lazy
              ("Program call failed with exit code " ^ string_of_int exit_code
             ^ ": !" ^ program ^ " "
              ^ String.concat " "
                  (List.map
                     (fun arg -> Value.pretty (StringV arg))
                     program_args)
              ^
              if stdout <> "" then "\n\nOutput: " ^ Util.abbreviate stdout
              else "") )))

(* This returns a runtime module *as well as a transformation for the ambient environment*.
   This is necessary since exceptions are flattened by the renamer so this needs to know about
   every possible exception definition. *)
let rec eval_mod_expr ~cap env = function
  | Import ((loc, _, body), _) ->
      (* TODO This ignores the header for now. I hope this is fine? *)
      let _, module_env =
        eval_seq_state ~cap `Statement (make_eval_env env.argv) body
      in
      ( { modules = module_env.module_vars; mod_vars = module_env.vars },
        fun ambient_env ->
          {
            ambient_env with
            exceptions =
              NameMap.union
                (fun _ _ x -> Some x)
                ambient_env.exceptions module_env.exceptions;
          } )
  | ModVar (loc, var) -> begin
      match VarMap.find_opt var env.module_vars with
      | None ->
          panic __LOC__
            (Loc.pretty loc ^ ": Module variable not found at runtime: '"
           ^ Name.pretty var ^ "'. This should have been caught way earlier!")
      | Some runtime_module -> (runtime_module, Fun.id)
    end
  | SubModule (loc, mod_expr, name) ->
      let runtime_module, ambient_env_trans = eval_mod_expr ~cap env mod_expr in
      begin
        match VarMap.find_opt name runtime_module.modules with
        | None ->
            panic __LOC__
              (Loc.pretty loc ^ ": Submodule not found at runtime: '"
             ^ Name.pretty name ^ "'. This should have been caught way earlier!"
              )
        | Some runtime_module -> (runtime_module, ambient_env_trans)
      end

and eval_statement ~cap (env : eval_env) (expr : Typed.expr) =
  match expr with
  | If (loc, condition_expr, then_expr, else_expr) ->
      let condition = eval_expr ~cap env condition_expr in
      begin
        match condition with
        | BoolV true -> eval_statement ~cap env then_expr
        | BoolV false -> eval_statement ~cap env else_expr
        | _ ->
            panic __LOC__
              (Loc.pretty loc ^ ": Non-bool in if condition at runtime: "
             ^ Value.pretty condition)
      end
      (* Single program calls are just evaluated like pipes *)
  | ProgCall (loc, _, _) as expr ->
      eval_statement ~cap env (Pipe (loc, [ expr ]))
  (* Pipes in statements inherit the parents stdout. *)
  (* Pipes without value inputs also inherit the parents stdin *)
  | Pipe (loc, (ProgCall _ :: _ as prog_exprs)) ->
      handle_process_exceptions loc env
        begin
          fun () ->
            let progs = progs_of_exprs ~cap env prog_exprs in
            let pid =
              Pipe.compose ~sw:cap.switch ~mgr:cap.mgr ~env:(full_env_vars env)
                progs
            in
            let status = Pipe.wait_to_status pid in
            env.last_status := status;
            if status <> 0 then begin
              let program, arguments = Base.List.last_exn progs in
              raise_command_failure_exception env loc program arguments status
                ""
            end
        end
  | Pipe (loc, expr :: prog_exprs) ->
      handle_process_exceptions loc env
        begin
          fun () ->
            let output_lines =
              Value.as_args
                (fun value ->
                  panic __LOC__
                    (Loc.pretty loc ^ ": Invalid process argument at runtime: "
                   ^ Value.pretty value))
                (eval_expr ~cap env expr)
            in

            let progs = progs_of_exprs ~cap env prog_exprs in

            let output_flow =
              Eio.Flow.string_source (String.concat "\n" output_lines)
            in

            let process =
              Pipe.compose_stdin ~mgr:cap.mgr ~sw:cap.switch
                ~env:(full_env_vars env) progs output_flow
            in
            let status = Pipe.wait_to_status process in
            env.last_status := status;
            if status <> 0 then begin
              let program, arguments = Base.List.last_exn progs in
              raise_command_failure_exception env loc program arguments status
                ""
            end
        end
  | Seq (_, exprs) ->
      let _ = eval_seq ~cap `Statement env exprs in
      ()
  | expr ->
      let _ = eval_expr ~cap env expr in
      ()

and eval_expr ~cap (env : eval_env) (expr : Typed.expr) : value =
  let open Typed in
  match expr with
  | Var (((loc, _ty), _), x) ->
      if x.index = Name.primop_index then PrimOpV x.name
      else lookup_var env loc x
  | DataConstructor (loc, name) -> PartialDataConV name
  | ExceptionConstructor (loc, name) -> begin
      match NameMap.find_opt name env.exceptions with
      | Some (parameter_names, env, message_expr) ->
          PartialExceptionV (name, parameter_names, env, message_expr)
      | None ->
          panic __LOC__
            (Loc.pretty loc ^ ": Exception constructor not found at runtime: "
           ^ Name.pretty name)
    end
  | VariantConstructor (loc, name, args) ->
      let arg_vals = List.map (eval_expr ~cap env) args in
      VariantConstructorV (name, arg_vals)
  | ModSubscriptDataCon (void, _, _, _) -> absurd void
  | App (loc, f, args) ->
      (* See Note [left-to-right evaluation] *)
      let f_val = eval_expr ~cap env f in
      let arg_vals = List.map (eval_expr ~cap env) args in
      eval_app ~cap env loc f_val arg_vals
  | Lambda (_, patterns, e) -> ClosureV (lazy env, patterns, e)
  | StringLit (_, s) -> StringV s
  | NumLit (_, f) -> NumV f
  | BoolLit (_, b) -> BoolV b
  | UnitLit _ -> unitV
  | ListLit (_, exprs) ->
      let vals = List.map (eval_expr ~cap env) exprs in
      ListV vals
  | TupleLit (_, exprs) ->
      let vals = Array.map (eval_expr ~cap env) (Array.of_list exprs) in
      TupleV vals
  | RecordLit (_, kvs) ->
      let kv_vals = List.map (fun (k, e) -> (k, eval_expr ~cap env e)) kvs in
      RecordV (RecordVImpl.of_list kv_vals)
  | StringInterpolation (_, components) ->
      let eval_component = function
        | StringComponent (_, str) -> str
        | Interpolation (_, exprs) -> begin
            match eval_seq ~cap `Expr env exprs with
            | StringV str -> str
            | (NumV _ | BoolV _) as value -> Value.pretty value
            | value ->
                panic __LOC__
                  ("Non-string value in string interpolation: "
                 ^ Value.pretty value)
          end
      in
      let strings = List.map eval_component components in
      StringV (String.concat "" strings)
  | Subscript (({ main = loc; _ }, _), map_expr, key) -> begin
      match eval_expr ~cap env map_expr with
      | RecordV map -> begin
          match RecordVImpl.find key map with
          | value :: _ -> value
          | [] ->
              panic __LOC__
                (Loc.pretty loc ^ ": Record does not contain key '" ^ key
               ^ "' at runtime")
        end
      | value ->
          panic __LOC__
            (Loc.pretty loc ^ ": Subscript to non-record at runtme: "
           ^ Value.pretty value)
    end
  | ModSubscript ((loc, _), mod_name, name) ->
      let runtime_module =
        match VarMap.find_opt mod_name env.module_vars with
        | None ->
            panic __LOC__
              (Loc.pretty loc ^ ": Module variable not found at runtime: '"
             ^ Name.pretty mod_name ^ "'. This should have been caught earlier!"
              )
        | Some runtime_module -> runtime_module
      in
      begin
        match VarMap.find_opt name runtime_module.mod_vars with
        | None ->
            panic __LOC__
              (Loc.pretty loc ^ ": Module member not found at runtime: '"
             ^ Name.pretty name ^ "'. This should have been caught earlier!")
        | Some reference -> reference
      end
  | RecordUpdate (loc, expr, update_exprs) -> begin
      match eval_expr ~cap env expr with
      | RecordV vals ->
          (* See note [Left to Right evaluation] *)
          let update_vals =
            List.map
              (fun (k, expr) -> (k, eval_expr ~cap env expr))
              update_exprs
          in
          let add_val (k, v) m =
            RecordVImpl.update k
              (function
                | _ :: vs -> v :: vs
                | [] -> panic __LOC__ "Empty value list in record update")
              m
          in
          RecordV (List.fold_right add_val update_vals vals)
      | value ->
          panic __LOC__
            ("Non-record value in record update: " ^ Value.pretty value)
    end
  | RecordExtension (loc, expr, ext_exprs) -> begin
      match eval_expr ~cap env expr with
      | RecordV vals ->
          (* See note [Left to Right evaluation] *)
          let update_vals =
            List.map (fun (k, expr) -> (k, eval_expr ~cap env expr)) ext_exprs
          in
          RecordV (RecordVImpl.add_list update_vals vals)
      | value ->
          panic __LOC__
            ("Non-record value in record update: " ^ Value.pretty value)
    end
  | DynLookup (loc, map_expr, key_expr) -> begin
      match eval_expr ~cap env map_expr with
      | ListV list -> begin
          match eval_expr ~cap env key_expr with
          | NumV num when Float.is_integer num ->
              let index = Float.to_int num in
              begin
                match List.nth_opt list index with
                | Some x -> x
                | None ->
                    raise
                      (EvalError
                         (IndexOutOfRange (list, index, loc :: env.call_trace)))
              end
          | value ->
              panic __LOC__
                (Loc.pretty loc ^ ": Invalid list index at runtime: "
               ^ Value.pretty value)
        end
      | value ->
          panic __LOC__
            (Loc.pretty loc ^ ": list index in non-list at runtime: "
           ^ Value.pretty value)
    end
  | BinOp (loc, e1, Add, e2) ->
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr ~cap env e1 in
      let v2 = eval_expr ~cap env e2 in
      begin
        match (v1, v2) with
        | NumV num1, NumV num2 -> NumV (num1 +. num2)
        | _ ->
            panic __LOC__
              (Loc.pretty loc ^ ": Trying to add non-numbers at runtime: "
             ^ Value.pretty v1 ^ ", " ^ Value.pretty v2)
      end
  | BinOp (loc, e1, Sub, e2) ->
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr ~cap env e1 in
      let v2 = eval_expr ~cap env e2 in
      begin
        match (v1, v2) with
        | NumV num1, NumV num2 -> NumV (num1 -. num2)
        | _ ->
            panic __LOC__
              (Loc.pretty loc ^ ": Trying to subtract non-numbers at runtime: "
             ^ Value.pretty v1 ^ ", " ^ Value.pretty v2)
      end
  | BinOp (loc, e1, Mul, e2) ->
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr ~cap env e1 in
      let v2 = eval_expr ~cap env e2 in
      begin
        match (v1, v2) with
        | NumV num1, NumV num2 -> NumV (num1 *. num2)
        | _ ->
            panic __LOC__
              (Loc.pretty loc ^ ": Trying to multiply non-numbers at runtime: "
             ^ Value.pretty v1 ^ ", " ^ Value.pretty v2)
      end
  | BinOp (loc, e1, Div, e2) ->
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr ~cap env e1 in
      let v2 = eval_expr ~cap env e2 in
      begin
        match (v1, v2) with
        | NumV num1, NumV num2 -> NumV (num1 /. num2)
        | _ ->
            panic __LOC__
              (Loc.pretty loc ^ ": Trying to divide non-numbers at runtime: "
             ^ Value.pretty v1 ^ ", " ^ Value.pretty v2)
      end
  | BinOp (loc, e1, Cons, e2) ->
      let v1 = eval_expr ~cap env e1 in
      let v2 = eval_expr ~cap env e2 in
      begin
        match v2 with
        | ListV values -> ListV (v1 :: values)
        | _ ->
            panic __LOC__
              (Loc.pretty loc ^ ": Trying to cons to non-list at runtime: "
             ^ Value.pretty v2)
      end
  | BinOp (loc, e1, Concat, e2) ->
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr ~cap env e1 in
      let v2 = eval_expr ~cap env e2 in
      begin
        match (v1, v2) with
        | ListV xs, ListV ys -> ListV (xs @ ys)
        | RecordV xs, RecordV ys -> RecordV (RecordVImpl.union xs ys)
        | StringV s1, StringV s2 -> StringV (s1 ^ s2)
        | StringV s1, NumV _ -> StringV (s1 ^ Value.pretty v2)
        | NumV _, StringV s2 -> StringV (Value.pretty v1 ^ s2)
        | _, _ ->
            panic __LOC__
              (Loc.pretty loc ^ ": Invalid args to concat at runtime: "
             ^ Value.pretty v1 ^ ", " ^ Value.pretty v2)
      end
  | BinOp (_, e1, Equals, e2) ->
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr ~cap env e1 in
      let v2 = eval_expr ~cap env e2 in
      BoolV (val_eq v1 v2)
  | BinOp (_, e1, NotEquals, e2) ->
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr ~cap env e1 in
      let v2 = eval_expr ~cap env e2 in
      BoolV (not (val_eq v1 v2))
  | BinOp (loc, e1, LE, e2) ->
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr ~cap env e1 in
      let v2 = eval_expr ~cap env e2 in

      begin
        match (v1, v2) with
        | NumV num1, NumV num2 -> BoolV (num1 <= num2)
        | _ ->
            panic __LOC__
              (Loc.pretty loc
             ^ ": Trying to apply <= to non-numbers at runtime: "
             ^ Value.pretty v1 ^ ", " ^ Value.pretty v2)
      end
  | BinOp (loc, e1, GE, e2) ->
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr ~cap env e1 in
      let v2 = eval_expr ~cap env e2 in
      begin
        match (v1, v2) with
        | NumV num1, NumV num2 -> BoolV (num1 >= num2)
        | _ ->
            panic __LOC__
              (Loc.pretty loc
             ^ ": Trying to apply >= to non-numbers at runtime: "
             ^ Value.pretty v1 ^ ", " ^ Value.pretty v2)
      end
  | BinOp (loc, e1, LT, e2) ->
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr ~cap env e1 in
      let v2 = eval_expr ~cap env e2 in
      begin
        match (v1, v2) with
        | NumV num1, NumV num2 -> BoolV (num1 < num2)
        | _ ->
            panic __LOC__
              (Loc.pretty loc
             ^ ": Trying to apply < to non-numbers at runtime: "
             ^ Value.pretty v1 ^ ", " ^ Value.pretty v2)
      end
  | BinOp (loc, e1, GT, e2) ->
      (* See Note [left-to-right evaluation] *)
      let v1 = eval_expr ~cap env e1 in
      let v2 = eval_expr ~cap env e2 in
      begin
        match (v1, v2) with
        | NumV num1, NumV num2 -> BoolV (num1 > num2)
        | _ ->
            panic __LOC__
              (Loc.pretty loc
             ^ ": Trying to apply > to non-numbers at runtime: "
             ^ Value.pretty v1 ^ ", " ^ Value.pretty v2)
      end
  | BinOp (loc, e1, Or, e2) -> begin
      match eval_expr ~cap env e1 with
      | BoolV true -> BoolV true
      | BoolV false -> begin
          match eval_expr ~cap env e2 with
          | BoolV true -> BoolV true
          | BoolV false -> BoolV false
          | value ->
              panic __LOC__
                (Loc.pretty loc ^ ": Non-bool in || expression at runtime: "
               ^ Value.pretty value)
        end
      | value ->
          panic __LOC__
            (Loc.pretty loc ^ ": Non-bool in || expression at runtime: "
           ^ Value.pretty value)
    end
  | BinOp (loc, e1, And, e2) -> begin
      match eval_expr ~cap env e1 with
      | BoolV false -> BoolV false
      | BoolV true -> begin
          match eval_expr ~cap env e2 with
          | BoolV true -> BoolV true
          | BoolV false -> BoolV false
          | value ->
              panic __LOC__
                (Loc.pretty loc ^ ": Non-bool in && expression at runtime: "
               ^ Value.pretty value)
        end
      | value ->
          panic __LOC__
            (Loc.pretty loc ^ ": Non-bool in && expression at runtime: "
           ^ Value.pretty value)
    end
  | Not (loc, e) -> begin
      match eval_expr ~cap env e with
      | BoolV b -> BoolV (not b)
      | value ->
          panic __LOC__
            (Loc.pretty loc ^ ": Non-bool in not expression at runtime: "
           ^ Value.pretty value)
    end
  | Range (loc, e1, e2) ->
      let start_val = eval_expr ~cap env e1 in
      let end_val = eval_expr ~cap env e2 in
      begin
        match (start_val, end_val) with
        | NumV start_num, NumV end_num ->
            let rec build_range acc x =
              if x < start_num then acc
              else build_range (NumV x :: acc) (x -. 1.)
            in
            ListV (build_range [] end_num)
        | _ ->
            panic __LOC__
              (Loc.pretty loc ^ ": Non-number in range bounds at runtime: "
             ^ Value.pretty start_val ^ ", " ^ Value.pretty end_val)
      end
  | ListComp (loc, result_expr, comp_exprs) ->
      ListV (eval_list_comp ~cap env loc result_expr comp_exprs)
  | If (loc, condition_expr, then_expr, else_expr) ->
      let condition = eval_expr ~cap env condition_expr in
      begin
        match condition with
        | BoolV true -> eval_expr ~cap env then_expr
        | BoolV false -> eval_expr ~cap env else_expr
        | _ ->
            panic __LOC__
              (Loc.pretty loc ^ ": Non-bool in if condition at runtime: "
             ^ Value.pretty condition)
      end
  | Seq (_, exprs) -> eval_seq ~cap `Expr env exprs
  | LetSeq _
  | LetRecSeq _
  | LetEnvSeq _
  | LetModuleSeq _
  | LetDataSeq _
  | LetTypeSeq _
  | LetExceptionSeq _ ->
      raise (Panic "let assignment found outside of sequence expression")
  | ProgCall (loc, prog, args) as expr ->
      eval_expr ~cap env (Pipe (loc, [ expr ]))
  | Pipe (loc, []) -> raise (Panic "empty pipe")
  | Pipe (loc, (ProgCall _ :: _ as exprs)) ->
      handle_process_exceptions loc env
        begin
          fun () ->
            let progs = progs_of_exprs ~cap env exprs in

            let result, status =
              Pipe.compose_stdout ~sw:cap.switch ~mgr:cap.mgr
                ~env:(full_env_vars env) progs
            in
            let result = trim_output result in

            env.last_status := status;
            if status <> 0 then begin
              let program, arguments = Base.List.last_exn progs in
              raise_command_failure_exception env loc program arguments status
                result
            end;
            StringV result
        end
  | Pipe (loc, expr :: exprs) ->
      handle_process_exceptions loc env
        begin
          fun () ->
            let output_lines =
              Value.as_args
                (fun value ->
                  panic __LOC__
                    (Loc.pretty loc ^ ": Invalid process argument at runtime: "
                   ^ Value.pretty value))
                (eval_expr ~cap env expr)
            in

            let progs = progs_of_exprs ~cap env exprs in

            let stdin_source =
              Eio.Flow.string_source (String.concat "\n" output_lines)
            in

            let result, status =
              Pipe.compose_in_out ~sw:cap.switch ~mgr:cap.mgr
                ~env:(full_env_vars env) progs stdin_source
            in
            let result = trim_output result in

            env.last_status := status;
            if status <> 0 then begin
              let program, arguments = Base.List.last_exn progs in
              raise_command_failure_exception env loc program arguments status
                result
            end;

            StringV result
        end
  | EnvVar (loc, var) -> begin
      (* We first check if the env var has been locally overriden by a
          'let $x = ...' expression. If it has not, we look for actual environment variables *)
      match EnvMap.find_opt var env.env_vars with
      | Some str -> StringV str
      | None -> begin
          match Sys.getenv_opt var with
          | None -> StringV ""
          | Some str -> StringV str
        end
    end
  | Async (loc, expr) ->
      let promise =
        Util.async_promise ~switch:cap.switch
          begin
            fun () -> eval_expr ~cap env expr
          end
      in
      PromiseV promise
  | Await (loc, expr) -> begin
      match eval_expr ~cap env expr with
      | PromiseV p -> Eio.Promise.await p
      | value ->
          panic __LOC__
            (Loc.pretty loc ^ ": Trying to await non-promise at runtime: "
           ^ Value.pretty value)
    end
  | Match (loc, scrut, branches) ->
      let scrut_val = eval_expr ~cap env scrut in
      let rec go = function
        | (pat, expr) :: branches -> begin
            match match_pat_opt pat scrut_val with
            | Some env_trans -> eval_expr ~cap (env_trans env) expr
            | None -> go branches
          end
        | [] ->
            raise
              (EvalError (NonExhaustiveMatch (scrut_val, loc :: env.call_trace)))
      in
      go branches
  | Ascription (_, expr, _) ->
      (* Ascriptions don't affect the evaluator.
         These have been checked by the type checker already *)
      eval_expr ~cap env expr
  | Unwrap (loc, expr) ->
      let value = eval_expr ~cap env expr in
      begin
        match value with
        | DataConV (_, underlying) -> underlying
        | RefV reference -> !reference
        | _ ->
            panic __LOC__
              (Loc.pretty loc
             ^ ": Trying to unwrap non data constructor value: "
             ^ Value.pretty value)
      end
  | MakeRef (loc, expr) ->
      let value = eval_expr ~cap env expr in
      RefV (ref value)
  | Assign (loc, ref_expr, expr) ->
      let ref_val = eval_expr ~cap env ref_expr in
      let value = eval_expr ~cap env expr in
      begin
        match ref_val with
        | RefV reference ->
            reference := value;
            unitV
        | _ ->
            panic __LOC__
              (Loc.pretty loc
             ^ ": Trying to assign to non-reference at runtime: "
             ^ Value.pretty value)
      end
  | Try (loc, try_expr, handlers) -> begin
      try
        Eio.Switch.run
          begin
            fun switch ->
              (* The try expression runs in a new switch, so that this try block
                 can catch exceptions from all threads.
                 This also keeps the semantics of try blocks waiting for all inner threads to return *)
              eval_expr ~cap:{ cap with switch } env try_expr
          end
      with
      | EvalError
          (PolarisException (exception_name, arguments, trace, lazy_message)) ->
          let rec go = function
            | (pattern, body) :: handlers -> begin
                match
                  match_pat_opt pattern
                    (ExceptionV (exception_name, arguments, trace, lazy_message))
                with
                (* This is the handler! *)
                | Some env_trans -> eval_expr ~cap (env_trans env) body
                (* This handler did not match, so we try the next one *)
                | None -> go handlers
              end
            | [] ->
                (* No handler matched so we rethrow the exception *)
                raise_notrace
                  (EvalError
                     (PolarisException
                        (exception_name, arguments, trace, lazy_message)))
          in
          go handlers
    end
  | Raise (loc, expr) -> begin
      match eval_expr ~cap env expr with
      | ExceptionV (name, arguments, trace, message) ->
          let trace =
            match trace with
            | NotYetRaised ->
                RaisedPreviously
                  { original_trace = loc :: env.call_trace; reraised = [] }
            | RaisedPreviously previous ->
                RaisedPreviously
                  { previous with reraised = loc :: previous.reraised }
          in
          raise_notrace
            (EvalError (PolarisException (name, arguments, trace, message)))
      | value ->
          panic __LOC__
            ("Trying to raise non-exception at runtime: " ^ Value.pretty value)
    end

and eval_app ~cap env loc fun_v arg_vals =
  match fun_v with
  | ClosureV (clos_env, params, body) ->
      (* Function arguments are evaluated left to right *)
      let env_trans = match_params params arg_vals (loc :: env.call_trace) in

      let updated_clos_env = env_trans (Lazy.force clos_env) in
      (* The call trace is extended and carried over to the closure environment *)
      eval_expr ~cap
        { updated_clos_env with call_trace = loc :: env.call_trace }
        body
  | PartialDataConV constructor_name -> begin
      match arg_vals with
      | [ x ] -> DataConV (constructor_name, x)
      | _ ->
          panic __LOC__
            ("Invalid number of arguments for data constructor '"
            ^ Name.pretty constructor_name
            ^ "': ["
            ^ String.concat ", " (List.map Value.pretty arg_vals)
            ^ "]")
    end
  | PartialExceptionV
      (exception_name, parameter_names, message_env, message_expr) -> begin
      match Base.List.zip parameter_names arg_vals with
      | Ok arguments ->
          let message =
            lazy
              begin
                let updated_env =
                  List.fold_right
                    (fun (param, value) env -> insert_var param value env)
                    arguments message_env
                in
                match eval_expr ~cap updated_env message_expr with
                | StringV exception_message -> exception_message
                | _ ->
                    panic __LOC__
                      (Loc.pretty loc
                     ^ ": Exception message evaluated to non-string at runtime"
                      )
              end
          in
          ExceptionV (exception_name, arguments, NotYetRaised, message)
      | Unequal_lengths -> todo __LOC__
    end
  | PrimOpV prim_name ->
      eval_primop ~cap
        { env with call_trace = loc :: env.call_trace }
        prim_name arg_vals loc
  | value ->
      panic __LOC__
        (Loc.pretty loc ^ ": Trying to apply non-function at runtime: "
       ^ Value.pretty value)

(* This takes a continuation argument in order to stay mutually tail recursive with eval_expr *)
and eval_seq_cont :
      'r.
      [ `Expr | `Statement ] ->
      cap:eval_capabilities ->
      eval_env ->
      Typed.expr list ->
      (eval_env -> (Typed.expr, value) either -> 'r) ->
      'r =
 fun context ~cap env exprs cont ->
  match exprs with
  | [] -> cont env (Right unitV)
  | LetSeq (loc, pat, e) :: exprs ->
      let scrut = eval_expr ~cap env e in
      let env_trans = match_pat pat scrut (loc :: env.call_trace) in
      eval_seq_cont ~cap context (env_trans env) exprs cont
      (* We can safely ignore the type annotation since it has been checked by the typechecker already *)
  | LetRecSeq (loc, _ty, f, params, e) :: exprs ->
      let rec env' = lazy (insert_var f (ClosureV (env', params, e)) env) in
      eval_seq_cont ~cap context (Lazy.force env') exprs cont
  | LetEnvSeq (loc, x, e) :: exprs ->
      let env' = insert_env_var loc x (eval_expr ~cap env e) env in
      eval_seq_cont ~cap context env' exprs cont
  | LetModuleSeq (loc, x, me) :: exprs ->
      let module_val, ambient_env_trans = eval_mod_expr ~cap env me in
      let env = ambient_env_trans (insert_module_var x module_val env) in
      eval_seq_cont ~cap context env exprs cont
  | (LetDataSeq (loc, _, _, _) | LetTypeSeq (loc, _, _, _)) :: exprs ->
      (* Types are erased at runtime, so we don't need to do anything clever here *)
      eval_seq_cont ~cap context env exprs cont
  | LetExceptionSeq (_loc, exception_name, params, message_expr) :: exprs ->
      let updated_env =
        {
          env with
          exceptions =
            VarMap.add exception_name
              (List.map fst params, env, message_expr)
              env.exceptions;
        }
      in
      eval_seq_cont ~cap context updated_env exprs cont
  | [ e ] -> cont env (Left e)
  | e :: exprs ->
      eval_statement ~cap env e;
      eval_seq_cont ~cap context env exprs cont

and eval_seq context ~cap (env : eval_env) (exprs : Typed.expr list) : value =
  let result =
    eval_seq_cont ~cap context env exprs (fun env expr_or_val ->
        match expr_or_val with
        | Left expr -> begin
            match context with
            | `Expr -> eval_expr ~cap env expr
            | `Statement ->
                eval_statement ~cap env expr;
                unitV
          end
        | Right value -> value)
  in
  result

and eval_seq_state ~cap context (env : eval_env) (exprs : Typed.expr list) :
    value * eval_env =
  let result =
    eval_seq_cont ~cap context env exprs (fun env expr_or_val ->
        match expr_or_val with
        | Left expr -> begin
            match context with
            | `Expr -> (eval_expr ~cap env expr, env)
            | `Statement ->
                eval_statement ~cap env expr;
                (unitV, env)
          end
        | Right value -> (value, env))
  in
  result

and eval_list_comp ~cap env loc result_expr = function
  | Typed.FilterClause expr :: comps -> begin
      match eval_expr ~cap env expr with
      | BoolV false -> []
      | BoolV true -> eval_list_comp ~cap env loc result_expr comps
      | value ->
          panic __LOC__
            (Loc.pretty loc
           ^ ": Non-bool in list comprehension filter at runtime: "
           ^ Value.pretty value)
    end
  | DrawClause (pattern, expr) :: comps -> begin
      match eval_expr ~cap env expr with
      | ListV values ->
          let eval_with_val v =
            match match_pat_opt pattern v with
            | None -> []
            | Some env_trans ->
                eval_list_comp ~cap (env_trans env) loc result_expr comps
          in
          List.concat_map eval_with_val values
      | value ->
          panic __LOC__
            (Loc.pretty loc ^ ": Non-list inlist comprehension at runtime: "
           ^ Value.pretty value)
    end
  | [] -> [ eval_expr ~cap env result_expr ]

and eval_primop ~cap env op args loc =
  (* TODO: intern primop names *)
  match op with
  | "print" ->
      let pretty_print = function
        | StringV str -> str
        | x -> Value.pretty x
      in
      print_endline (String.concat " " (List.map pretty_print args));
      unitV
  | "head" -> begin
      match args with
      | [ ListV (head :: tail) ] -> head
      | [ ListV [] ] ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ("head", args, "Empty list", loc :: env.call_trace)))
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ("head", args, "Expected a single list", loc :: env.call_trace)))
    end
  | "tail" -> begin
      match args with
      | [ ListV (head :: tail) ] -> ListV tail
      | [ ListV [] ] ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ("tail", args, "Empty list", loc :: env.call_trace)))
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ("tail", args, "Expected a single list", loc :: env.call_trace)))
    end
  | "cons" -> begin
      match args with
      | [ x; ListV xs ] -> ListV (x :: xs)
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ( "cons",
                    args,
                    "Expected an element and a list",
                    loc :: env.call_trace )))
    end
  | "lines" -> begin
      match args with
      | [ StringV "" ] -> ListV []
      | [ StringV arg ] ->
          ListV (List.map (fun s -> StringV s) (String.split_on_char '\n' arg))
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ( "lines",
                    args,
                    "Expected a single string",
                    loc :: env.call_trace )))
    end
  | "chars" -> begin
      match args with
      | [ StringV arg ] ->
          ListV
            (List.of_seq
               (Seq.map
                  (fun char ->
                    StringV (String.of_seq (Seq.cons char Seq.empty)))
                  (String.to_seq arg)))
      | _ -> panic __LOC__ "invalid arguments to 'chars' primop"
    end
  | "split" -> begin
      match args with
      | [ _; StringV "" ] -> ListV []
      | [ StringV sep_str; StringV arg ] when String.length sep_str = 1 ->
          let sep = String.get sep_str 0 in
          ListV (List.map (fun s -> StringV s) (String.split_on_char sep arg))
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ( "split",
                    args,
                    "Expected (char, string)",
                    loc :: env.call_trace )))
    end
  | "replace" -> begin
      match args with
      | [ StringV needle; StringV repl; StringV str_v ] ->
          StringV
            (Re.replace_string (Re.compile (Re.str needle)) ~by:repl str_v)
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ( "replace",
                    args,
                    "Expected three strings",
                    loc :: env.call_trace )))
    end
  | "regexpReplace" -> begin
      match args with
      | [ StringV pattern; StringV repl; StringV str ] ->
          let regexp = Re.Pcre.regexp pattern in
          StringV (Re.replace_string regexp ~by:repl str)
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ( "regexpReplace",
                    args,
                    "Expected three strings",
                    loc :: env.call_trace )))
    end
  | "regexpMatch" -> begin
      match args with
      | [ StringV pattern; StringV arg ] ->
          let regexp = Re.Pcre.regexp pattern in
          begin
            try
              let results = Re.matches regexp arg in
              ListV (List.map (fun x -> StringV x) results)
            with
            | Not_found -> ListV []
          end
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ( "regexpMatch",
                    args,
                    "Expected (string, string)",
                    loc :: env.call_trace )))
    end
  | "regexpMatchGroups" -> begin
      match args with
      | [ StringV pattern; StringV arg ] ->
          let regexp = Re.Pcre.regexp pattern in
          begin
            try
              let results = Re.all regexp arg in
              ListV
                (List.map
                   (fun group ->
                     ListV
                       (Array.to_list
                          (Array.map (fun x -> StringV x) (Re.Group.all group))))
                   results)
            with
            | Not_found -> ListV []
          end
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ( "regexpMatchGroups",
                    args,
                    "Expected (string, string)",
                    loc :: env.call_trace )))
    end
  | "regexpTransform" -> begin
      match args with
      | [ StringV pattern; transformClos; StringV str_v ] ->
          let regexp = Re.Pcre.regexp pattern in

          let transform group =
            match
              eval_app ~cap env loc transformClos
                [ StringV (Re.Group.get group 0) ]
            with
            | StringV repl -> repl
            | value ->
                panic __LOC__
                  (Loc.pretty loc
                 ^ ": regexpTransform: Replacement function did not return a \
                    string. Returned value: " ^ Value.pretty value)
          in

          StringV (Re.replace regexp ~f:transform str_v)
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ( "regexpTransform",
                    args,
                    "Expected (string, function, string)",
                    loc :: env.call_trace )))
    end
  | "regexpTransformAll" -> begin
      match args with
      | [ StringV pattern; transformClos; StringV str_v ] ->
          let regexp = Re.Pcre.regexp pattern in

          let transform group =
            match
              eval_app ~cap env loc transformClos
                [
                  ListV
                    (List.map
                       (fun x -> StringV x)
                       (Array.to_list (Re.Group.all group)));
                ]
            with
            | StringV repl -> repl
            | value ->
                panic __LOC__
                  (Loc.pretty loc
                 ^ ": regexpTransform: Replacement function did not return a \
                    string. Returned value: " ^ Value.pretty value)
          in

          StringV (Re.replace regexp ~f:transform str_v)
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ( "regexpTransformAll",
                    args,
                    "Expected (string, function, string)",
                    loc :: env.call_trace )))
    end
  | "writeFile" -> begin
      match args with
      | [ StringV path; StringV content ] ->
          let path = Eio.Path.(cap.fs / path) in

          let default_permissions = 0b0110100100 in
          Eio.Path.save path content ~create:(`Or_truncate default_permissions);
          unitV
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ( "writeFile",
                    args,
                    "Expected two string arguments",
                    loc :: env.call_trace )))
    end
  | "parseInt" -> begin
      match args with
      | [ StringV arg_str ] -> begin
          match int_of_string_opt (String.trim arg_str) with
          | Some int_val -> NumV (float_of_int int_val)
          | None ->
              raise
                (EvalError
                   (PrimOpArgumentError
                      ( "parseInt",
                        args,
                        "Expected a string containing an integer literal",
                        loc :: env.call_trace )))
        end
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ("parseInt", args, "Expected a string", loc :: env.call_trace)))
    end
  | "parseNum" -> begin
      match args with
      | [ StringV arg_str ] -> begin
          match float_of_string_opt (String.trim arg_str) with
          | Some float_val -> NumV float_val
          | None ->
              raise
                (EvalError
                   (PrimOpArgumentError
                      ( "parseNum",
                        args,
                        "Expected a string containing a numeric literal",
                        loc :: env.call_trace )))
        end
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ("parseNum", args, "Expected a strings", loc :: env.call_trace)))
    end
  | "readLine" ->
      let prompt =
        match args with
        | [] -> ""
        | [ StringV prompt ] -> prompt
        | _ ->
            raise
              (EvalError
                 (PrimOpArgumentError
                    ( "readLine",
                      args,
                      "Expected no arguments or a single string",
                      loc :: env.call_trace )))
      in
      begin
        match Bestline.bestline prompt with
        | Some input -> VariantConstructorV ("Just", [ StringV input ])
        | None -> VariantConstructorV ("Nothing", [])
      end
  | "chdir" -> begin
      match args with
      | [ StringV path_str ] ->
          Sys.chdir path_str;
          unitV
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ("chdir", args, "Expected a strings", loc :: env.call_trace)))
    end
  | "exit" -> begin
      match args with
      | [ NumV arg ] -> exit (int_of_float arg)
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ("exit", args, "Expected an integer", loc :: env.call_trace)))
    end
  | "toString" -> begin
      match args with
      | [ arg ] -> StringV (Value.pretty arg)
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ( "toString",
                    args,
                    "Expected a single argument",
                    loc :: env.call_trace )))
    end
  | "getArgv" -> begin
      match args with
      | [] -> ListV (List.map (fun x -> StringV x) env.argv)
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ( "getArgv",
                    args,
                    "Expected no arguments",
                    loc :: env.call_trace )))
    end
  | "getEnv" -> begin
      match args with
      | [ StringV var ] -> begin
          match Sys.getenv_opt var with
          | None -> VariantConstructorV ("Nothing", [])
          | Some value -> VariantConstructorV ("Just", [ StringV value ])
        end
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ( "getEnv",
                    args,
                    "Expected a single string",
                    loc :: env.call_trace )))
    end
  | "fail" -> begin
      match args with
      | [ StringV msg ] ->
          raise (EvalError (RuntimeError (msg, loc :: env.call_trace)))
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ("fail", args, "Expected a string", loc :: env.call_trace)))
    end
  | "scriptLocal" -> begin
      match args with
      | [ StringV path ] ->
          StringV (Filename.dirname (List.hd env.argv) ^ "/" ^ path)
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ("fail", args, "Expected a string", loc :: env.call_trace)))
    end
  | "commandExists" -> begin
      match args with
      | [ StringV path ] -> BoolV (Util.command_exists path)
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ( "commandExists",
                    args,
                    "Expected a string",
                    loc :: env.call_trace )))
    end
  | "ensure" -> begin
      match args with
      | [ StringV path ] ->
          if Util.command_exists path then unitV
          else raise (EvalError (EnsureFailed (path, loc :: env.call_trace)))
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ("ensure", args, "Expected a string", loc :: env.call_trace)))
    end
  | "status" -> NumV (Float.of_int !(env.last_status))
  | "mod" -> begin
      match args with
      | [ NumV x; NumV y ] ->
          NumV (float_of_int (int_of_float x mod int_of_float y))
      | _ ->
          raise
            (EvalError
               (PrimOpArgumentError
                  ("mod", args, "Expected two integers", loc :: env.call_trace)))
    end
  | "floor" -> begin
      match args with
      | [ NumV x ] -> NumV (floor x)
      | _ ->
          panic __LOC__
            (Loc.pretty loc ^ ": floor: Non-number argument passed at runtime")
    end
  | "ceil" -> begin
      match args with
      | [ NumV x ] -> NumV (ceil x)
      | _ ->
          panic __LOC__
            (Loc.pretty loc ^ ": ceil: Non-number argument passed at runtime")
    end
  | "exceptionMessage" -> begin
      match args with
      | [ ExceptionV (_, _, _, message) ] -> StringV (Lazy.force message)
      | _ ->
          panic __LOC__
            (Loc.pretty loc
           ^ ": exceptionMessage: Non-exception passed at runtime")
    end
  | "compareString" -> begin
      match args with
      | [ StringV string1; StringV string2 ] -> begin
          match String.compare string1 string2 with
          | x when x < 0 -> VariantConstructorV ("Less", [])
          | x when x > 0 -> VariantConstructorV ("Greater", [])
          | _ -> VariantConstructorV ("Equal", [])
        end
      | _ ->
          panic __LOC__
            (Loc.pretty loc ^ ": compareString: Non-strings passed at runtime")
    end
  | _ -> raise (Panic ("Invalid or unsupported primop: " ^ op))

and progs_of_exprs ~cap env = function
  | ProgCall (loc, progName, args) :: exprs ->
      let fail value =
        panic __LOC__
          (Loc.pretty loc ^ ": Invalid process argument at runtime : "
         ^ Value.pretty value)
      in
      let arg_strings =
        List.concat_map
          (fun arg -> Value.as_args fail (eval_expr ~cap env arg))
          args
      in
      (progName, arg_strings) :: progs_of_exprs ~cap env exprs
  | expr :: exprs ->
      panic __LOC__ (Loc.pretty (get_loc expr) ^ ": Non-program call in pipe")
  | [] -> []

and make_eval_env (argv : string list) : eval_env =
  {
    vars = VarMap.empty;
    env_vars = EnvMap.empty;
    argv;
    call_trace = [];
    last_status = ref 0;
    module_vars = VarMap.empty;
    exceptions = VarMap.empty;
  }

let eval ~cap (argv : string list) (exprs : Typed.expr list) : value =
  eval_seq `Expr ~cap (make_eval_env argv) exprs

let flag_info_of_flag_def (env_ref : eval_env ref) (flag_def : Typed.flag_def) :
    Argparse.flag_info =
  let aliases = flag_def.flags in
  let description = Option.value ~default:"" flag_def.description in
  match flag_def.args with
  | Switch name ->
      env_ref := insert_var name (BoolV false) !env_ref;
      {
        aliases;
        arg_count = 0;
        required = false;
        description;
        action = (fun _ -> env_ref := insert_var name (BoolV true) !env_ref);
      }
  | Varargs name ->
      env_ref := insert_var name (ListV []) !env_ref;
      {
        aliases;
        arg_count = 1;
        required = false;
        description;
        action =
          (fun args ->
            match lookup_var !env_ref Loc.internal name with
            | ListV prev_args ->
                (* TODO: This is quadratic :/ *)
                env_ref :=
                  insert_var name
                    (ListV (prev_args @ List.map (fun x -> StringV x) args))
                    !env_ref
            | v ->
                raise
                  (Panic
                     ("flag_info_of_flag_def: Non-list value in varargs flag \
                       reference: " ^ Value.pretty v)));
      }
  | Named params ->
      {
        aliases;
        arg_count = List.length params;
        required = true;
        description;
        action =
          (fun args ->
            List.iter2
              (fun param arg ->
                env_ref := insert_var param (StringV arg) !env_ref)
              params args);
      }
  | NamedDefault params ->
      List.iter
        (fun (param, default) ->
          env_ref := insert_var param (StringV default) !env_ref)
        params;
      {
        aliases;
        arg_count = List.length params;
        required = false;
        description;
        action =
          (fun args ->
            List.iter2
              (fun (param, _default) arg ->
                env_ref := insert_var param (StringV arg) !env_ref)
              params args);
      }

let eval_header (env : eval_env) (header : Typed.header) : eval_env =
  let description = Option.value ~default:"" header.description in
  let usage = Option.value ~default:"[OPTIONS]" header.usage in

  let env_ref = ref env in

  let infos = List.map (flag_info_of_flag_def env_ref) header.options in

  let prog_info = Argparse.{ name = List.hd env.argv; description; usage } in

  let args =
    Argparse.run infos prog_info
      (fun msg -> raise (EvalError (ArgParseError msg)))
      (List.tl env.argv)
  in

  { !env_ref with argv = List.hd env.argv :: args }

(* Note [left-to-right evaluation]
   We sometimes have to explicitly bind variables to intermediary
   variables, since OCaml's evaluation order is right-to-left,
   but we would like to enforce left-to-right evaluation.
   Example:
   ```
   let v1 = eval_expr ~cap env e1
   let v2 = eval_expr ~cap env e2
   f v1 v2
   ```
   instead of
   ```
   f (eval_expr ~cap env e1) (eval_expr ~cap env e2)
   ```
*)
