open Syntax
open Syntax.Renamed
open Util

let _tc_category, trace_tc = Trace.make ~flag:"types" ~prefix:"Types"
let _emit_category, trace_emit = Trace.make ~flag:"emit" ~prefix:"Emit"
let _unify_category, trace_unify = Trace.make ~flag:"unify" ~prefix:"Unify"
let _subst_category, trace_subst = Trace.make ~flag:"subst" ~prefix:"Subst"
let _class_category, trace_class = Trace.make ~flag:"class" ~prefix:"Class"

type unify_context = ty * ty

type type_error =
  | UnableToUnify of (ty * ty) * unify_context option
  (* ^           ^ full original types (None if there is no difference) *)
  (* | specific mismatch                                                *)
  | DifferentVariantConstrArgs of string * ty list * ty list * unify_context
  | MismatchedTyCon of name * name * unify_context option
  | Impredicative of (ty * ty) * unify_context option
  | OccursCheck of ty Typeref.t * name * ty * unify_context option
  | FunctionsWithDifferentArgCounts of ty list * ty list * unify_context
  | PassedIncorrectNumberOfArgsToFun of int * ty list * ty
  | IncorrectNumberOfArgsInLambda of int * ty list * ty
  | NonProgCallInPipe of expr
  | MissingRecordFields of
      (string * ty) list * (string * ty) list * unify_context
  | MissingVariantConstructors of
      (string * ty list) list * (string * ty list) list * unify_context
  | ArgCountMismatchInDefinition of name * ty list * int
  | NonFunTypeInLetRec of name * ty
  | CannotUnwrapNonData of ty
  | ValueRestriction of ty
  | SkolemUnifyEscape of ty * ty * ty * unify_context option
    (* ^    ^    ^ unified type
       |    | skolem
       | unif *)
  | DataConUnifyEscape of ty * name * ty * unify_context option
  | IncorrectNumberOfExceptionArgs of name * int * ty list
  | PatternError of Pattern.pattern_error
  | MissingInstance of class_constraint
  | TupleLiteralOfWrongLength of int * ty array
  | NonProgramArgument of ty
  | NonInterpolatable of ty
  | AmbiguousClassConstraint of class_constraint * (name list * ty list) list

exception TypeError of loc * type_error

module TyperefOrd = struct
  type t = ty Typeref.t * name

  let compare (ref1, _) (ref2, _) =
    Unique.compare (Typeref.get_unique ref1) (Typeref.get_unique ref2)
end

module TyperefSet = Set.Make (TyperefOrd)
module TyperefMap = Map.Make (TyperefOrd)

let normalize_unif = Ty.normalize_unif

type class_instance = {
  evidence : Evidence.source
}

(** An efficient trie for matching type classes during type class resolution
    This is indexed by the unrolled type constructors for instance parameters
    making instance matching extremely efficient in practice.

    Matching instances traverses all matching paths at the same time, which allows
    it to report ambiguities*)
module InstanceTrie : sig
  type t

  val empty : t
  val add_instance : name list -> ty list -> class_instance -> t -> t

  val match_instance :
    t ->
    ty list ->
    [> `Found of class_instance | `Missing | `Ambiguous of class_instance list ]
end = struct
  type type_head =
    (* DeBruijn level of a universal type class variable *)
    | HVariable of int
    (* Number of parameters. This is necessary to keep the linearly unrolled
       type string consistent *)
    | HFunction of int
    | HConstraint
    (* This has to use Name.t instead of name for the deriver to work correctly *)
    | HConstructor of Name.t
    (* HUnifs will never be substituted here, so we only need to carry
        their `Unique.t`s, rather than their full `Typeref.t`s.
        The actual matching logic treats these abstractly, just like skolems *)
    | HUnif of Unique.t
    | HSkolem of Unique.t
    | HNumber
    | HBool
    | HString
    | HException
    | HTuple of int
    | HList
    | HRef
    | HPromise
    (* record fields are presented in canonical (sorted) order (up to duplicate labels)*)
    | HRecord of string array
    | HVariant of (string * int) array
  [@@deriving ord]

  (* TODO: Unroll incrementally (maybe by returning a type_head Seq.t?) *)
  (* This needs to take an environment because it might need to follow type aliases :/
     (Maybe type aliases should just contain their definition so we don't need this anymore) *)
  let unroll : name list -> ty list -> type_head list =
   fun universals types ->
    let rec unroll_type = function
      | Forall _ -> panic __LOC__ "forall in type class instance"
      | Fun (arguments, result) ->
          Difflist.cons
            (HFunction (List.length arguments))
            (Difflist.append
               (Difflist.concat_map_list unroll_type arguments)
               (unroll_type result))
      | Constraint (constraint_type, ty) ->
          Difflist.cons HConstraint
            (Difflist.append (unroll_type constraint_type) (unroll_type ty))
      (* TODO: Resolve DeBruijn levels based on the position in the type, *not* the quantifier*)
      | TyVar name -> begin
          match
            Base.List.findi
              ~f:(fun _ universal_name -> Name.equal name universal_name)
              universals
          with
          | Some (index, _) -> Difflist.cons (HVariable index) Difflist.empty
          | None ->
              panic __LOC__
                ("Trying to unroll a type involving the unbound universal type \
                  variable '" ^ Name.pretty name ^ "'")
        end
      | TyConstructor (name, arguments) ->
          Difflist.cons (HConstructor name)
            (Difflist.concat_map_list unroll_type arguments)
      | TypeAlias { underlying; _ } ->
          (* type aliases are immediately substituted by their definition *)
          unroll_type underlying
      | Unif (typeref, _) ->
          Difflist.cons (HUnif (Typeref.get_unique typeref)) Difflist.empty
      | Skol (unique, _, _) ->
          (* TODO: Do we need to check skolem levels here? *)
          Difflist.cons (HSkolem unique) Difflist.empty
      | Number -> Difflist.cons HNumber Difflist.empty
      | Bool -> Difflist.cons HBool Difflist.empty
      | String -> Difflist.cons HString Difflist.empty
      | Exception -> Difflist.cons HException Difflist.empty
      | Tuple elements ->
          Difflist.cons
            (HTuple (Array.length elements))
            (Difflist.concat_map_list unroll_type (Array.to_list elements))
      | List ty -> Difflist.cons HList (unroll_type ty)
      | Ref ty -> Difflist.cons HRef (unroll_type ty)
      | Promise ty -> Difflist.cons HPromise (unroll_type ty)
      | RecordClosed fields ->
          let sorted_fields =
            Util.immutable_stable_sort
              (fun (key1, _) (key2, _) -> String.compare key1 key2)
              fields
          in
          Difflist.cons
            (HRecord (Array.map fst sorted_fields))
            (Difflist.concat_map_array
               (fun (_, ty) -> unroll_type ty)
               sorted_fields)
      | RecordSkol (fields, (unique, _, _)) ->
          let sorted_fields =
            Util.immutable_stable_sort
              (fun (key1, _) (key2, _) -> String.compare key1 key2)
              fields
          in
          Difflist.cons
            (HRecord (Array.map fst sorted_fields))
            (Difflist.snoc
               (Difflist.concat_map_array
                  (fun (_, ty) -> unroll_type ty)
                  sorted_fields)
               (HSkolem unique))
      | RecordVar _ ->
          panic __LOC__
            "Uninstantiated (record) type variable in wanted constraint"
      | RecordUnif (fields, (typeref, _)) ->
          let sorted_fields =
            Util.immutable_stable_sort
              (fun (key1, _) (key2, _) -> String.compare key1 key2)
              fields
          in
          Difflist.cons
            (HRecord (Array.map fst sorted_fields))
            (Difflist.snoc
               (Difflist.concat_map_array
                  (fun (_, ty) -> unroll_type ty)
                  sorted_fields)
               (HUnif (Typeref.get_unique typeref)))
      | VariantClosed constructors ->
          let sorted_constructors =
            Util.immutable_stable_sort
              (fun (key1, _) (key2, _) -> String.compare key1 key2)
              constructors
          in
          Difflist.cons
            (HVariant
               (Array.map
                  (fun (key, types) -> (key, List.length types))
                  sorted_constructors))
            (Difflist.concat_map_array
               (fun (_, types) -> Difflist.concat_map_list unroll_type types)
               sorted_constructors)
      | VariantSkol (constructors, (unique, _, _)) ->
          let sorted_constructors =
            Util.immutable_stable_sort
              (fun (key1, _) (key2, _) -> String.compare key1 key2)
              constructors
          in
          Difflist.cons
            (HVariant
               (Array.map
                  (fun (key, types) -> (key, List.length types))
                  sorted_constructors))
            (Difflist.snoc
               (Difflist.concat_map_array
                  (fun (_, types) -> Difflist.concat_map_list unroll_type types)
                  sorted_constructors)
               (HSkolem unique))
      | VariantUnif (constructors, (typeref, _)) ->
          let sorted_constructors =
            Util.immutable_stable_sort
              (fun (key1, _) (key2, _) -> String.compare key1 key2)
              constructors
          in
          Difflist.cons
            (HVariant
               (Array.map
                  (fun (key, types) -> (key, List.length types))
                  sorted_constructors))
            (Difflist.snoc
               (Difflist.concat_map_array
                  (fun (_, types) -> Difflist.concat_map_list unroll_type types)
                  sorted_constructors)
               (HUnif (Typeref.get_unique typeref)))
      | VariantVar _ ->
          panic __LOC__
            "Uninstantiated (variant) type variable in wanted constraint"
      | ModSubscriptTyCon _ -> .
    in

    Difflist.to_list (Difflist.concat_map_list unroll_type types)

  module HeadMap = Map.Make (struct
    type t = type_head

    let compare = compare_type_head
  end)

  type t =
    | Leaf of class_instance
    | Node of t HeadMap.t

  (* TODO: This technically does not allow nullary instances. Those are really weird though, so... *)
  let empty = Node HeadMap.empty

  let add_instance variables types instance trie =
    let rec create_trie_from = function
      | [] -> Leaf instance
      | head :: rest ->
          Node (HeadMap.add head (create_trie_from rest) HeadMap.empty)
    in
    let rec go children = function
      | head :: rest -> begin
          match HeadMap.find_opt head children with
          | Some (Leaf _) ->
              panic __LOC__
                "duplicate type class instance or misshapen instance trie"
          | Some (Node children) ->
              let child_trie = go children rest in
              Node (HeadMap.add head child_trie children)
          | None -> 
            (* TODO: Should this be (head :: rest) or just rest? *)
            create_trie_from (head :: rest)
        end
      | [] -> panic __LOC__ "misshapen instance trie"
    in
    match trie with
    | Leaf _ ->
        panic __LOC__
          "duplicate nullary type class instance (what's wrong with you???)"
    | Node children -> go children (unroll variables types)

  let all_matching : ty list -> t -> (t * ty list) list =
   fun types trie ->
    let filter_map_or_substitute remaining_types children predicate =
      List.filter_map
        (fun (head, value) ->
          match head with
          | HVariable i -> todo __LOC__
          | head when predicate head -> Some (value, remaining_types)
          | _ -> None)
        (HeadMap.bindings children)
    in
    let normalize_first_unif = function
      | ty :: rest -> normalize_unif ty :: rest
      | [] -> []
    in

    match trie with
    | Leaf _ -> []
    | Node children -> begin
        match normalize_first_unif types with
        | [] -> todo __LOC__
        | RecordClosed _ :: rest
        | RecordVar _ :: rest
        | RecordUnif _ :: rest
        | RecordSkol _ :: rest
        | VariantClosed _ :: rest
        | VariantVar _ :: rest
        | VariantUnif _ :: rest
        | VariantSkol _ :: rest ->
            todo __LOC__ (* the difficult cases*)
        | Forall _ :: rest -> panic __LOC__ "forall in wanted class constraint"
        | Fun (parameters, result) :: rest ->
            let remaining_types = parameters @ [ result ] @ rest in
            let parameter_count = List.length parameters in
            filter_map_or_substitute remaining_types children (function
              | HFunction count when count = parameter_count -> true
              | _ -> false)
        | Constraint (constraint_type, result) :: rest ->
            let remaining_types = constraint_type :: result :: rest in
            filter_map_or_substitute remaining_types children (function
              | HConstraint -> true
              | _ -> false)
        | TyVar name :: rest ->
            panic __LOC__
              ("Uninstantiated type variable in wanted constraint: "
             ^ Name.pretty name)
        | TyConstructor (name, arguments) :: rest ->
            filter_map_or_substitute (arguments @ rest) children (function
              | HConstructor constr_name when Name.equal name constr_name ->
                  true
              | _ -> false)
        | TypeAlias _ :: rest -> todo __LOC__
        | Unif (typeref, _) :: rest ->
            filter_map_or_substitute rest children (function
              | HUnif unique
                when Unique.equal (Typeref.get_unique typeref) unique ->
                  true
              | _ -> false)
        | Skol (unique, _level, _) :: rest ->
            filter_map_or_substitute rest children (function
              | HSkolem head_unique when Unique.equal unique head_unique -> true
              | _ -> false)
        | Number :: rest ->
            filter_map_or_substitute rest children (function
              | HNumber -> true
              | _ -> false)
        | Bool :: rest ->
            filter_map_or_substitute rest children (function
              | HBool -> true
              | _ -> false)
        | String :: rest ->
            filter_map_or_substitute rest children (function
              | HString -> true
              | _ -> false)
        | Exception :: rest ->
            filter_map_or_substitute rest children (function
              | HException -> true
              | _ -> false)
        | Tuple arguments :: rest ->
            filter_map_or_substitute
              (Array.to_list arguments @ rest)
              children
              (function
                | HTuple count when count = Array.length arguments -> true
                | _ -> false)
        | List argument :: rest ->
            filter_map_or_substitute (argument :: rest) children (function
              | HList -> true
              | _ -> false)
        | Ref argument :: rest ->
            filter_map_or_substitute (argument :: rest) children (function
              | HRef -> true
              | _ -> false)
        | Promise argument :: rest ->
            filter_map_or_substitute (argument :: rest) children (function
              | HPromise -> true
              | _ -> false)
        | ModSubscriptTyCon _ :: _ -> .
      end

  let match_instance trie types =
    (* It might seem like it would be more efficient to traverse the trie in
       BFS rather than backtracking, but since we need to traverse every
       reachable path anyway to detect and report ambiguities, that actually turns
       out to be false!
       Depth first search is much simpler so that is what we do here. *)
    let rec go trie types =
      let subtries_and_remaining_types = all_matching types trie in

      let continue (subtrie, remaining) =
        match remaining with
        | [] -> begin
            match subtrie with
            | Leaf instance -> [ instance ]
            | Node _ -> panic __LOC__ "misshapen instance trie"
          end
        | _ -> go subtrie remaining
      in

      List.concat_map continue subtries_and_remaining_types
    in
    match go trie types with
    | [] -> `Missing
    | [ instance ] -> `Found instance
    | instances -> `Ambiguous instances
end

(* Note [Env in Constraints]
   During unification, we need to check for escaping type constructors.
   Unfortunately, unlike skolems, type constructors do *not* carry the level at which they were defined!
   We only resolve levels in the type checker, so if we were to carry the environment, we
   would need to separate the definitions of Renamed.ty and Typed.ty, which is painful to say the least.

   Instead, we keep constructor levels in the typing environment and make unification constraints
   carry around the environment at which they were emitted.
   This environment is only used to look up type constructors -- other lookups are covered by the (top-level) environment
   at the point of constraint solving already, since this one might contain additional definitions that creep into
   the constraint solving processs through other unification variables

   Other constraints also need to carry the environment since these are solved in terms of
   unification constraints
*)

type given_constraints = { class_instances : InstanceTrie.t NameMap.t }

type ty_constraint =
  (* See Note [Env in Constraints] *)
  | Unify of loc * ty * ty * local_env (* See Note [Env in Constraints] *)
  | Unwrap of loc * ty * ty * local_env (* See Note [Env in Constraints] *)
  | ProgramArg of loc * ty * local_env (* See Note [Env in Constraints] *)
  | RefineVariant of loc * ty * Pattern.path * string * ty * local_env
    (* See Note [Env in Constraints] *)
  | Interpolatable of loc * ty * local_env
  | WantedClass of {
      loc : loc;
      wanted : class_constraint;
      givens : given_constraints;
      local_env : local_env;
      evidence_target : Evidence.target;
    }

(* forall a. Eq(List(a)) *)
and global_env = {
  var_types : Typed.ty NameMap.t;
  module_var_contents : global_env NameMap.t;
  data_definitions : (name list * Typed.ty) NameMap.t;
  type_aliases : (name list * Typed.ty) NameMap.t;
  ambient_level : Typeref.level;
  exception_definitions : ty list NameMap.t;
  type_classes : name list NameMap.t;
  given_constraints : given_constraints;
}

and local_env = {
  local_types : ty NameMap.t;
  constraints : ty_constraint Difflist.t ref;
  given_constraints : given_constraints;
  module_var_contents : global_env NameMap.t;
  data_definitions : (Typeref.level * name list * ty) NameMap.t;
  type_aliases : (name list * ty) NameMap.t;
  (* We do not need carry type information about the type class methods here.
     These have already been matched up with the correct instance methods by the renamer *)
  type_classes : name list NameMap.t;
  level : Typeref.level;
  exception_definitions : ty list NameMap.t;
}

let unify : local_env -> loc -> ty -> ty -> unit =
 fun env loc ty1 ty2 ->
  trace_emit (lazy (Typed.pretty_type ty1 ^ " ~ " ^ Typed.pretty_type ty2));
  match (normalize_unif ty1, normalize_unif ty2) with
  (* We can skip obviously equal constraints.
     This will make debugging vastly simpler and might improve performance a little. *)
  | Number, Number
  | Bool, Bool
  | String, String
  | Exception, Exception
  | RecordClosed [||], RecordClosed [||] ->
      ()
  | TyVar name1, TyVar name2 when Name.equal name1 name2 -> ()
  | Unif (typeref1, _), Unif (typeref2, _) when Typeref.equal typeref1 typeref2
    ->
      ()
  | Skol (u1, level1, _), Skol (u2, level2, _) when Unique.equal u1 u2 ->
      assert (level1 = level2);
      ()
  | ty1, ty2 ->
      env.constraints :=
        Difflist.snoc !(env.constraints) (Unify (loc, ty1, ty2, env))

let unwrap_constraint : local_env -> loc -> ty -> ty -> unit =
 fun env loc ty1 ty2 ->
  trace_emit
    (lazy ("Unwrap " ^ Typed.pretty_type ty1 ^ " ==> " ^ Typed.pretty_type ty2));
  env.constraints :=
    Difflist.snoc !(env.constraints) (Unwrap (loc, ty1, ty2, env))

let prog_arg_constraint : local_env -> loc -> ty -> unit =
 fun env loc ty ->
  trace_emit (lazy ("Arg " ^ Typed.pretty_type ty));
  env.constraints :=
    Difflist.snoc !(env.constraints) (ProgramArg (loc, ty, env))

let interpolatable_constraint : local_env -> loc -> ty -> unit =
 fun env loc ty ->
  trace_emit (lazy ("Interpolatable " ^ Typed.pretty_type ty));
  env.constraints :=
    Difflist.snoc !(env.constraints) (Interpolatable (loc, ty, env))

let wanted_class :
    local_env -> loc -> class_constraint -> Evidence.target -> unit =
 fun env loc class_constraint target ->
  trace_emit
    (lazy
      ("[W] ∀"
      ^ String.concat " " (List.map Name.pretty class_constraint.variables)
      ^ ". "
      ^ Name.pretty class_constraint.class_name
      ^ "("
      ^ String.concat ", " (List.map pretty_type class_constraint.arguments)
      ^ ")"));
  env.constraints :=
    Difflist.snoc !(env.constraints)
      (WantedClass
         {
           loc;
           wanted = class_constraint;
           givens = env.given_constraints;
           local_env = env;
           evidence_target = target;
         })

let fresh_unif_raw_with env raw_name =
  let typeref = Typeref.make env.level in
  (typeref, Name.{ name = raw_name; index = Typeref.get_unique typeref })

let fresh_unif_raw env = fresh_unif_raw_with env "α"

let fresh_unif env =
  let typeref, name = fresh_unif_raw env in
  Unif (typeref, name)

let fresh_unif_with env name =
  let typeref, name = fresh_unif_raw_with env name in
  Unif (typeref, name)

let fresh_skolem_raw_with env name =
  let unique = Unique.fresh () in
  (unique, env.level, name)

let fresh_skolem_with env name =
  let unique = Unique.fresh () in
  Skol (unique, env.level, name)

let refine_variant : local_env -> loc -> ty -> Pattern.path -> string -> ty =
 fun env loc ty path name ->
  let result_type = fresh_unif env in
  trace_emit
    (lazy
      ("RefineVariant (" ^ Typed.pretty_type ty ^ ", _, " ^ name ^ ") ==> "
      ^ Typed.pretty_type result_type));
  env.constraints :=
    Difflist.snoc !(env.constraints)
      (RefineVariant (loc, ty, path, name, result_type, env));
  result_type

let increase_ambient_level env =
  { env with level = Typeref.next_level env.level }

(* Process the continuation in a higher level *)
let enter_level env cont = cont (increase_ambient_level env)

(* Note [Levels]

   In HM-style type inference, unification variables can only ever be generalized
   if they do not occur free in the environment.
   Generalization is only sound because the generalized unification variable is never bound and hence
   any possible instantiation is safe.
   If the variable comes from an outer scope, it might still be bound by unification later on,
   violating this assumption and dismantling type safety in the process.

   One might assume that this cannot happen in Polaris, since we never implicitly generalize local bindings,
   but it is unfortunately not that simple.

   1) Because polaris supports mutable references, we need a value restriction that blocks generalization
     of non-value expressions. The residual unification variables from this are enough to implement unsafeCoerce.
     (See test/categories/types/outer_gen.pls for an example)

   2) Skolems follow the same rules about not escaping the scope of their binder, though unlike unification variables,
     these can be introduced explicitly in inner let bindings with explicit type signatures.
     (See test/categories/types/skolem_escape.pls for an example)

   The naive solution here is to follow Algorithm W to a tee and (linearly!) search the entire environment every
   time a binding is generalized.
   While simple, this crushes any hopes of efficient inference, since it increases the running complexity of the
   type checker to O(n^2), which is simply unacceptable.

   Levels provide a saner way to check for escaping variables.
   Levels, defined in `typeref.ml` start at `Typeref.top_level` and the ambient level carried in a `local_env` is
   increased in the body of every let binding using `enter_level`.

   Now, unification variables can only ever be generalized at the level where they are bound and skolems
   are invalid (because they would escape) if they occur at a lower level than the one where they were bound.

   Additionally, unification needs to update levels when unifying unification variables.
   It needs to traverse0the full type to perform an occurs check anyway, so we can merge the two
   traversals inside `bind_directly`.

   The approach taken here mostly follows sound_eager from https://okmij.org/ftp/ML/generalization.html.
*)

let insert_var : name -> ty -> local_env -> local_env =
 fun x ty env -> { env with local_types = NameMap.add x ty env.local_types }

let insert_data_definition :
    Typeref.level -> name -> name list -> ty -> local_env -> local_env =
 fun level constructor params underlying_type env ->
  {
    env with
    data_definitions =
      NameMap.add constructor
        (level, params, underlying_type)
        env.data_definitions;
  }

let insert_type_alias : name -> name list -> ty -> local_env -> local_env =
 fun constructor params underlying_type env ->
  {
    env with
    type_aliases =
      NameMap.add constructor (params, underlying_type) env.type_aliases;
  }

let insert_type_class : name -> name list -> local_env -> local_env =
 fun class_name parameter_names env ->
  {
    env with
    type_classes = NameMap.add class_name parameter_names env.type_classes;
  }

let pretty_ty_constraint = function
  | Unify (_, ty1, ty2, _) -> pretty_type ty1 ^ " ~ " ^ pretty_type ty2
  | Unwrap (_, ty1, ty2, _) ->
      "Unwrap(" ^ pretty_type ty1 ^ ") --> " ^ pretty_type ty2
  | ProgramArg (_, argument_type, _) ->
      "ProgramArg(" ^ pretty_type argument_type ^ ")"
  | RefineVariant _ -> "RefineVariant(_)"
  | Interpolatable (_, argument_type, _) ->
      "Interpolatable(" ^ pretty_type argument_type ^ ")"
  | WantedClass { wanted; _ } ->
      Name.pretty wanted.class_name
      ^ "("
      ^ String.concat ", " (List.map pretty_type wanted.arguments)
      ^ ")"

let emit_givens loc givens (env : local_env) =
  let add_given given_constraints (given, evidence, entailed) =
    trace_emit
      (lazy
        ("[G] ∀"
        ^ String.concat " " (List.map Name.pretty given.variables)
        ^ ". "
        ^ Name.pretty given.class_name
        ^ "("
        ^ String.concat ", " (List.map pretty_type given.arguments)
        ^ ") ==> ("
        ^ String.concat ", " (List.map pretty_class_constraint entailed)
        ^ ") #d = "
        ^ Evidence.display_source evidence));

    let class_instances =
      NameMap.update given.class_name
        (fun maybe_instance_trie ->
          let instance_trie =
            Option.value maybe_instance_trie ~default:InstanceTrie.empty
          in

          let instance = { evidence } in

          Some
            (InstanceTrie.add_instance given.variables given.arguments instance
               instance_trie))
        given_constraints.class_instances
    in
    { given_constraints with class_instances }
  in
  let given_constraints =
    List.fold_left add_given env.given_constraints givens
  in
  { env with given_constraints }

let insert_exception_definition : name -> ty list -> local_env -> local_env =
 fun name params env ->
  {
    env with
    exception_definitions = NameMap.add name params env.exception_definitions;
  }

let replace_tvar : name -> ty -> ty -> ty =
 fun name_to_replace replacement_type ->
  Traversal.transform_type
    begin
      fun ty ->
        match normalize_unif ty with
        | TyVar name when Name.equal name name_to_replace -> replacement_type
        | ty -> ty
    end

let replace_tvars : ty NameMap.t -> ty -> ty =
 fun vars ->
  Traversal.transform_type
    begin
      function
      | TyVar tv -> begin
          match NameMap.find_opt tv vars with
          | None -> TyVar tv
          | Some ty -> ty
        end
      | ty -> ty
    end

(* This needs to collect a list of *atomic* sources and a separate list
   of sources *to be abstracted*, since compound sources might need to be split and rebuilt *)
let rec collect_atomic_given_constraints :
    ty -> (class_constraint * Evidence.source) list * Evidence.source list =
  function
  | TyConstructor (name, arguments) ->
      let source = Evidence.make_source () in
      ( [ ({ variables = []; class_name = name; arguments }, source) ],
        [ source ] )
  | TypeAlias { name = _; arguments = _; underlying } ->
      collect_atomic_given_constraints underlying
  | Tuple [||] -> ([], [])
  | ty ->
      panic __LOC__
        ("Non type constructor in constraint: " ^ Typed.pretty_type ty)

(* This needs to collect a list of *atomic* targets and a separate list
   of targets *to be applied*, since compound targets might need to be split and rebuilt *)
let rec collect_atomic_wanted_constraints :
    local_env ->
    ty ->
    (class_constraint * Evidence.target) list * Evidence.target list =
 fun env -> function
  | TyConstructor (name, arguments) ->
      let target = Evidence.make_empty_target () in

      ( [ ({ variables = []; class_name = name; arguments }, target) ],
        [ target ] )
  | TypeAlias { name = _; arguments = _; underlying } ->
      collect_atomic_wanted_constraints env underlying
  | Tuple [||] -> ([], [])
  | ty ->
      panic __LOC__
        ("Non type constructor in constraint: " ^ Typed.pretty_type ty)

let rec instantiate_with_function :
    loc -> local_env -> ty -> ty * (ty -> ty) * Evidence.target list list =
 fun loc env ty ->
  match normalize_unif ty with
  | Forall (tv, ty) ->
      let unif = Unif (Typeref.make env.level, tv) in
      (* TODO: Collect all tyvars first to avoid multiple traversals *)
      let replacement_fun = replace_tvar tv unif in
      let instantiated, inner_replacement_fun, evidence =
        instantiate_with_function loc env (replacement_fun ty)
      in
      ( instantiated,
        (fun ty -> inner_replacement_fun (replacement_fun ty)),
        evidence )
  | Constraint (class_constraint, ty) ->
      let wanteds, targets_to_apply =
        collect_atomic_wanted_constraints env class_constraint
      in
      List.iter
        (fun (wanted, target) -> wanted_class env loc wanted target)
        wanteds;

      let instantiated, inner_replacement_fun, evidence =
        instantiate_with_function loc env ty
      in
      (* TODO: Let's hope the order of expressions is correct here *)
      (instantiated, inner_replacement_fun, targets_to_apply :: evidence)
  | TypeAlias { name = _; arguments = _; underlying } ->
      instantiate_with_function loc env underlying
  | ty -> (ty, Fun.id, [])

let instantiate : loc -> local_env -> ty -> ty * Evidence.target list list =
 fun loc env ty ->
  let instaniated, _, evidence = instantiate_with_function loc env ty in
  (instaniated, evidence)

let rec skolemize_with_function :
    loc ->
    local_env ->
    ty ->
    ty * (ty -> ty) * (local_env -> local_env) * Evidence.source list list =
 fun loc env ty ->
  match normalize_unif ty with
  | Forall (tv, ty) ->
      enter_level env
        begin
          fun env ->
            let skol = Skol (Unique.fresh (), env.level, tv) in
            let replacement_fun = replace_tvar tv skol in
            let skolemized, inner_replacement_fun, inner_trans, evidence =
              skolemize_with_function loc env (replacement_fun ty)
            in
            ( skolemized,
              (fun ty -> inner_replacement_fun (replacement_fun ty)),
              increase_ambient_level << inner_trans,
              evidence )
        end
  | Constraint (class_constraint, ty) ->
      let givens, sources_to_abstract =
        collect_atomic_given_constraints class_constraint
      in
      let givens =
        List.map
          (fun (class_constraint, evidence_source) ->
            (class_constraint, evidence_source, []))
          givens
      in
      let skolemized, skolemizer, env_transformer, evidence =
        skolemize_with_function loc env ty
      in
      ( skolemized,
        skolemizer,
        emit_givens loc givens << env_transformer,
        sources_to_abstract :: evidence )
  | TypeAlias { name; arguments; underlying } ->
      skolemize_with_function loc env underlying
  | ty -> (ty, Fun.id, Fun.id, [])

let skolemize : loc -> local_env -> ty -> ty * Evidence.source list list =
 fun loc env ty ->
  let skolemized, _, _, evidence = skolemize_with_function loc env ty in
  (skolemized, evidence)

(* `subsumes env loc ty1 ty2` asserts that ty1 is meant to be a subtype of ty2.
   Unlike, say, most object oriented languages, Polaris only has one notion of
   subtyping: polytypes are subtypes of more specific types.
   For example, forall a. a -> a is a subtype of Number -> Number *)
let subsumes :
    local_env ->
    loc ->
    ty ->
    ty ->
    Evidence.source list list * Evidence.target list list =
 fun env loc sub_type super_type ->
  trace_emit
    (lazy (Typed.pretty_type sub_type ^ " <= " ^ Typed.pretty_type super_type));

  let super_type, source_evidence = skolemize loc env super_type in
  let sub_type, target_evidence = instantiate loc env sub_type in
  unify env loc sub_type super_type;
  (source_evidence, target_evidence)

(* Check that an expression is syntactically a value and can be generalized.
   We might be able to extend this in the future, similar to OCaml's relaxed value restriction *)
let rec is_value : expr -> bool = function
  | Var _ -> true
  | DataConstructor _ -> true
  | ModSubscriptDataCon (void, _, _, _) -> .
  | VariantConstructor (_, _, args) -> List.for_all is_value args
  (* Applications of data constructors to values are safe *)
  | App (_, DataConstructor _, args) -> List.for_all is_value args
  | App _ -> false
  | Lambda _ -> true
  | StringLit _
  | NumLit _
  | BoolLit _
  | UnitLit _ ->
      true
  | StringInterpolation (_, components) ->
      let comp_is_value = function
        | StringComponent _ -> true
        | Interpolation (_, exprs) -> List.for_all is_value exprs
      in
      List.for_all comp_is_value components
  | ListLit (_, args)
  | TupleLit (_, args) ->
      List.for_all is_value args
  | RecordLit (_, args) -> List.for_all (fun (_, expr) -> is_value expr) args
  | Subscript (_, expr, _) -> is_value expr
  | ModSubscript _ -> true
  | RecordUpdate (_, base_expr, updates)
  | RecordExtension (_, base_expr, updates) ->
      is_value base_expr
      && List.for_all (fun (_, expr) -> is_value expr) updates
  | DynLookup (_, list_expr, index_expr) ->
      is_value list_expr && is_value index_expr
  | BinOp (_, left, _, right) -> is_value left && is_value right
  | Not (_, expr) -> is_value expr
  | Range (_, first, last) -> is_value first && is_value last
  | ListComp (_, list_expr, clauses) ->
      let clause_is_value = function
        | DrawClause (_, expr) -> is_value expr
        | FilterClause expr -> is_value expr
      in
      is_value list_expr && List.for_all clause_is_value clauses
  | If (_, condition, then_branch, else_branch) ->
      is_value condition && is_value then_branch && is_value else_branch
  | Seq (_, exprs) -> List.for_all is_value exprs
  | LetSeq (_, _, expr)
  | LetEnvSeq (_, _, expr) ->
      is_value expr
  | LetRecSeq _
  | LetTypeSeq _
  | LetDataSeq _
  | LetClassSeq _
  | LetInstanceSeq _ ->
      true
  (* TODO: This might be a bit too restrictive. Program calls cannot return polymorphic values anyway
     so we should be able to generalize e.g. the type of ([], !ls) to forall a. (List(a), String) *)
  | ProgCall _
  | Pipe _ ->
      false
  | EnvVar _ -> true
  | Async (_, expr) -> is_value expr
  | Await (_, expr) -> is_value expr
  | Match (_, scrutinee, cases) ->
      is_value scrutinee && List.for_all (fun (_, expr) -> is_value expr) cases
  | LetModuleSeq _ -> true
  (* TODO: As long as the ascribed type is monomorphic, this should be fine, right? *)
  | Ascription (_, expr, ty) -> is_value expr
  | Unwrap (_, expr) -> is_value expr
  (* TODO: Again, expressions containing assignments can safely be generalized right? ...right? Am I loosing my mind here? *)
  | Assign _ -> false
  (* This one right here, officer! *)
  | MakeRef _ -> false
  | LetExceptionSeq _ -> true
  (* Unapplied exception constructors are values (just like DataConstructors) *)
  | ExceptionConstructor _ -> true
  | Try (_, try_expr, handlers) ->
      is_value try_expr
      && List.for_all (fun (_, expr) -> is_value expr) handlers
  (* TODO: Are we allowed to generalize over the result of raise?
     If we are, do we need to check that the raised value is a value?
     It's never going to be returned anyway. *)
  | Raise (_, _expr) -> true
  | ExprExt (_, void) -> .

let binds_value : expr -> bool = function
  | LetSeq (_, _, expr) -> is_value expr
  | LetRecSeq _ -> true
  | _ -> false

let rec is_polytype : ty -> bool = function
  | TypeAlias { name = _; arguments = _; underlying } -> is_polytype underlying
  | Forall _ -> true
  | _ -> false

let rec eval_module_env :
    local_env -> module_expr -> global_env * Typed.module_expr =
 fun env -> function
  | ModVar (loc, var) -> begin
      match NameMap.find_opt var env.module_var_contents with
      | None ->
          panic __LOC__
            (Loc.pretty loc ^ ": Module variable not found in typechecker: '"
           ^ Name.pretty var ^ "'. This should have been caught earlier!")
      | Some contents -> (contents, ModVar (loc, var))
    end
  | Import ((loc, mod_exports, exprs), path) ->
      ( {
          var_types = mod_exports.exported_variable_types;
          (* TODO: Allow modules to export other modules and include them here *)
          module_var_contents = NameMap.empty;
          data_definitions = mod_exports.exported_data_definitions;
          exception_definitions = mod_exports.exported_exceptions;
          type_aliases = mod_exports.exported_type_aliases;
          type_classes = NameMap.map fst mod_exports.exported_type_classes;
          ambient_level = Typeref.initial_top_level;
          given_constraints = (* TODO *)
                              { class_instances = NameMap.empty };
        },
        Import ((loc, mod_exports, exprs), path) )
  | SubModule (loc, mod_expr, name) -> (
      let parent_env, mod_expr = eval_module_env env mod_expr in
      match NameMap.find_opt name parent_env.module_var_contents with
      | None ->
          panic __LOC__
            (Loc.pretty loc ^ ": Submodule not found in typechecker: '"
           ^ Name.pretty name ^ "'. This should have been caught earlier!")
      | Some contents -> (contents, SubModule (loc, mod_expr, name)))

(* Used in traces if --print-type-levels is enabled *)
let level_prefix env =
  if Config.print_levels () then "[" ^ Typeref.pretty_level env.level ^ "]: "
  else ""

let rec infer_pattern :
    local_env ->
    bool ->
    pattern ->
    ty * (local_env -> local_env) * Typed.pattern =
 fun env allow_polytype pat ->
  trace_tc
    (lazy (level_prefix env ^ "inferring pattern '" ^ pretty_pattern pat ^ "'"));
  match pat with
  | VarPat (loc, varname) ->
      let var_ty = fresh_unif env in
      (var_ty, insert_var varname var_ty, VarPat ((loc, var_ty), varname))
  | AsPat (loc, pattern, name) ->
      let pattern_ty, env_trans, pattern =
        infer_pattern env allow_polytype pattern
      in
      ( pattern_ty,
        insert_var name pattern_ty << env_trans,
        AsPat (loc, pattern, name) )
  | ConsPat (loc, head_pattern, tail_pattern) ->
      let elem_ty = fresh_unif env in
      let head_trans, head_pattern =
        check_pattern env allow_polytype head_pattern elem_ty
      in
      let tail_trans, tail_pattern =
        check_pattern (head_trans env) allow_polytype tail_pattern
          (List elem_ty)
      in
      ( List elem_ty,
        head_trans << tail_trans,
        ConsPat (loc, head_pattern, tail_pattern) )
  | ListPat (loc, patterns) ->
      let elem_ty = fresh_unif env in
      let transformers, patterns =
        List.split
          (List.map
             (fun p -> check_pattern env allow_polytype p elem_ty)
             patterns)
      in
      (List elem_ty, Util.compose transformers, ListPat (loc, patterns))
  | TuplePat (loc, patterns) ->
      let pattern_types, transformers, patterns =
        Util.split3 (List.map (infer_pattern env allow_polytype) patterns)
      in
      ( Tuple (Array.of_list pattern_types),
        Util.compose transformers,
        TuplePat (loc, patterns) )
  | NumPat (loc, number) -> (Number, Fun.id, NumPat (loc, number))
  | StringPat (loc, literal) -> (String, Fun.id, StringPat (loc, literal))
  | OrPat (loc, left, right) ->
      let left_ty, left_trans, left = infer_pattern env allow_polytype left in
      (* TODO: Make sure both sides bind the same set of variables with the same types *)
      let right_trans, right = check_pattern env allow_polytype right left_ty in
      (left_ty, left_trans << right_trans, OrPat (loc, left, right))
  | TypePat (loc, pattern, ty) ->
      (* TODO: Should this skolemize ty? *)
      let env_trans, pattern = check_pattern env allow_polytype pattern ty in

      (ty, env_trans, TypePat (loc, pattern, ty))
  | DataPat (loc, constructor_name, pattern) ->
      let datacon_level, type_variables, underlying_type_raw =
        match NameMap.find_opt constructor_name env.data_definitions with
        | Some (level, vars, ty) -> (level, vars, ty)
        | None ->
            panic __LOC__
              (Loc.pretty loc ^ ": Data constructor not found in typechecker: '"
              ^ Name.pretty constructor_name
              ^ "'. This should have been caught earlier!")
      in
      let vars_with_unifs =
        List.map (fun var -> (var, fresh_unif_with env var.name)) type_variables
      in

      let data_type =
        TyConstructor (constructor_name, List.map snd vars_with_unifs)
      in

      let underlying_type =
        replace_tvars
          (NameMap.of_seq (List.to_seq vars_with_unifs))
          underlying_type_raw
      in

      let env_trans, pattern =
        check_pattern env allow_polytype pattern underlying_type
      in

      (data_type, env_trans, DataPat (loc, constructor_name, pattern))
  | VariantPat (loc, constructor_name, patterns) ->
      (* See Note [Inferring Variant Patterns] *)
      let pattern_types, env_transformers, patterns =
        Util.split3 (List.map (infer_pattern env allow_polytype) patterns)
      in

      let extension_unif = fresh_unif_raw env in

      let variant_type =
        VariantUnif ([| (constructor_name, pattern_types) |], extension_unif)
      in

      ( variant_type,
        Util.compose env_transformers,
        VariantPat ((loc, variant_type), constructor_name, patterns) )
  | ExceptionDataPat (loc, name, patterns) -> begin
      match NameMap.find_opt name env.exception_definitions with
      | None ->
          panic __LOC__
            (Loc.pretty loc ^ ": Unbound exception in type checker: "
           ^ Name.pretty name)
      | Some param_types -> begin
          match Base.List.zip patterns param_types with
          | Unequal_lengths ->
              raise
                (TypeError
                   ( loc,
                     IncorrectNumberOfExceptionArgs
                       (name, List.length patterns, param_types) ))
          | Ok patterns_and_types ->
              let env_transformers, patterns =
                List.split
                  (List.map
                     (fun (pattern, ty) -> check_pattern env false pattern ty)
                     patterns_and_types)
              in
              ( Exception,
                Util.compose env_transformers,
                ExceptionDataPat (loc, name, patterns) )
        end
    end

(* Note [Inferring Variant Patterns]
   Inference for variant patterns is quite complicated.

   Naively, one might think that a pattern for constructor `A
   should just be inferred to have the closed variant type type < A() >, since this is exactly what it matches.

   While this would work quite well for let-bound patterns, this breaks down for match expressions.
   Every pattern in a match expression is checked against the type of the scrutinee, meaning every pattern
   needs to have the same type.

   For example, with this approach, the following, obviously sensible declaration would be rejected

   let f(x) = match x {
     A -> 0
     B -> 1
     _ -> 2
   }

   The obvious type for `f` would be `forall r. < A, B | r > -> Number`.
   This leads to the natural conclusion that variant patterns (e.g `A) should always be inferred to
   an *open* variant type instead (< A | ?r >).

   With this, the example above infers the correct type, but at the cost of breaking correctness!

   let f(x) = match x {
     A -> 0
     B -> 1
   }

   This declaration should be inferred to have type `< A, B > -> Number`, but this approach infers the
   open type `forall r. < A, B | r > -> Number` even thoough the function has no way to handle
   any constructors besides `A and `B!

   Now, how do we infer patterns such that variants are always open when they could match additional
   constructors at runtime and closed when they could not?

   We cheat.

   Instead of determining this through type inference and unification, we always infer open variant types
   and later apply separate exhaustiveness analysis to the match expression to close any rows that would be
   non-exhaustive with an open variant type.
   This is the same approach that is taken by OCaml (https://github.com/ocaml/ocaml/blob/trunk/typing/parmatch.ml#L1392)
*)

and check_pattern :
    local_env ->
    bool ->
    pattern ->
    ty ->
    (local_env -> local_env) * Typed.pattern =
 fun env allow_polytype pattern expected_ty ->
  trace_tc
    (lazy
      (level_prefix env ^ "checking pattern '" ^ pretty_pattern pattern ^ "' : "
     ^ pretty_type expected_ty));

  if (not allow_polytype) && is_polytype expected_ty then begin
    raise (TypeError (get_pattern_loc pattern, ValueRestriction expected_ty))
  end;

  let defer_to_inference () =
    let ty, trans, pattern = infer_pattern env allow_polytype pattern in
    let sources, targets =
      subsumes env (Typed.get_pattern_loc pattern) ty expected_ty
    in
    (* infer_pattern only infers monotypes (TODO: does it?!)*)
    assert (targets = []);
    assert (sources = []);
    (* TODO: I don't think this holds entirely right now. I guess we should just not allow polytypes in patterns?
       No idea how we would abstract the sources here otherwise.
       Concretely, how would we desugar `let (x : forall a. Eq a => a -> a, y) = (\x -> x, 5)`? *)
    (trans, pattern)
  in
  match (pattern, expected_ty) with
  (* We need a special case for var patterns to allow polymorphic type patterns.
     (These would be rejected by unification) *)
  | VarPat (loc, var), expected_ty ->
      (insert_var var expected_ty, VarPat ((loc, expected_ty), var))
  | AsPat (loc, pattern, name), expected_ty ->
      let env_trans, pattern =
        check_pattern env allow_polytype pattern expected_ty
      in
      (insert_var name expected_ty << env_trans, AsPat (loc, pattern, name))
  | ConsPat (loc, head, tail), List elem_ty ->
      let head_trans, head = check_pattern env allow_polytype head elem_ty in
      let tail_trans, tail =
        check_pattern env allow_polytype tail (List elem_ty)
      in
      (tail_trans << head_trans, ConsPat (loc, head, tail))
  | ConsPat (loc, _, _), _ -> defer_to_inference ()
  | ListPat (loc, patterns), List elem_ty ->
      let transformers, patterns =
        List.split
          (List.map
             (fun pat -> check_pattern env allow_polytype pat elem_ty)
             patterns)
      in
      (Util.compose transformers, ListPat (loc, patterns))
  | ListPat _, _ -> defer_to_inference ()
  | TuplePat (loc, patterns), Tuple arg_tys ->
      let transformers, patterns =
        match
          Base.List.map2 patterns (Array.to_list arg_tys)
            ~f:(check_pattern env allow_polytype)
        with
        | Ok transformers_and_patterns -> List.split transformers_and_patterns
        | Unequal_lengths ->
            raise
              (TypeError
                 (loc, TupleLiteralOfWrongLength (List.length patterns, arg_tys)))
      in
      (Util.compose transformers, TuplePat (loc, patterns))
  | TuplePat _, _ -> defer_to_inference ()
  | NumPat (loc, number), Number -> (Fun.id, NumPat (loc, number))
  | NumPat _, _ -> defer_to_inference ()
  | StringPat (loc, literal), String -> (Fun.id, StringPat (loc, literal))
  | StringPat _, _ -> defer_to_inference ()
  | OrPat (loc, left, right), _ ->
      let left_trans, left =
        check_pattern env allow_polytype left expected_ty
      in
      let right_trans, right =
        check_pattern env allow_polytype right expected_ty
      in
      (right_trans << left_trans, OrPat (loc, left, right))
  | TypePat (loc, pattern, ty), expected_ty ->
      if (not allow_polytype) && is_polytype ty then begin
        raise (TypeError (loc, ValueRestriction ty))
      end;

      let sources, targets = subsumes env loc ty expected_ty in
      assert (sources = []);
      assert (targets = []);
      (* TODO: Yeah no these are *not* empty *)
      check_pattern env allow_polytype pattern ty
  | ( DataPat (loc, data_name, underlying_pattern),
      TyConstructor (constr_name, args) )
    when Name.equal data_name constr_name -> begin
      match NameMap.find_opt data_name env.data_definitions with
      | None -> panic __LOC__ "Unbound data constructor in type checker"
      | Some (_datacon_level, type_params, underlying_type_raw) ->
          let underlying_type =
            replace_tvars
              (NameMap.of_seq
                 (Seq.zip (List.to_seq type_params) (List.to_seq args)))
              underlying_type_raw
          in
          let env_trans, underlying_pattern =
            check_pattern env allow_polytype underlying_pattern underlying_type
          in
          (env_trans, DataPat (loc, data_name, underlying_pattern))
    end
  | DataPat _, _ -> defer_to_inference ()
  | VariantPat _, _ -> defer_to_inference ()
  | ExceptionDataPat _, _ -> defer_to_inference ()

(** Split a function type into its components.
    If the type is not statically known to be a function, this is achieved by unifying
    with a function type consisting of type variables (This is why the number of arguments has to be known beforehand). *)

(* TODO: Also split off constraints here *)

let rec split_fun_ty : local_env -> loc -> int -> ty -> ty list * ty =
 fun env loc arg_count ty ->
  assert (not (is_polytype ty));
  match normalize_unif ty with
  | Fun (args, result) ->
      (* TODO: Should we check that the argument count matches here? *)
      (args, result)
  | TypeAlias { name = _; arguments = _; underlying } ->
      (* We continue recursively, unwrapping every layer of type synonym until we hit either a function
         or a non-alias type that we can unify with a function type.
         This guarantees that type synonyms for functions taking polytypes behave correctly *)
      split_fun_ty env loc arg_count underlying
  | ty ->
      let argument_types = List.init arg_count (fun _ -> fresh_unif env) in
      let result_type = fresh_unif env in
      (* TODO: Is the order correct here? Does it even matter?
         If this is known to be a function (or function alias), we handle it separately
         anyways, so this case should only be triggered (and not fail no matter what we do)
         if the function is a unification variable *)
      let sources, targets =
        subsumes env loc ty (Fun (argument_types, result_type))
      in
      assert (sources = []);
      assert (targets = []);
      (argument_types, result_type)

(** Find the type under a reference. This will match on the type directly if possible or
    use unification otherwise *)
let rec split_ref_ty : local_env -> loc -> ty -> ty =
 fun env loc ty ->
  assert (not (is_polytype ty));
  match normalize_unif ty with
  | Ref ty -> ty
  | TypeAlias { underlying; _ } -> split_ref_ty env loc underlying
  | ty ->
      let inner_type = fresh_unif env in
      let sources, targets = subsumes env loc ty (Ref inner_type) in
      assert (sources = []);
      assert (targets = []);
      inner_type

let rec infer : local_env -> expr -> ty * Typed.expr =
 fun env expr ->
  trace_tc
    (lazy (level_prefix env ^ "inferring expression '" ^ pretty expr ^ "'"));
  match expr with
  | Var (loc, x) -> begin
      match NameMap.find_opt x env.local_types with
      | Some ty ->
          let instantiated_type, targets = instantiate loc env ty in
          ( instantiated_type,
            apply_targets loc targets (Typed.Var ((loc, ty), x)) )
      | None ->
          panic __LOC__
            ("Unbound variable in type checker: '" ^ Name.pretty x ^ "'")
    end
  | DataConstructor (loc, data_name) -> begin
      match NameMap.find_opt data_name env.data_definitions with
      | Some (_datacon_level, params, ty) ->
          let data_constructor_type =
            List.fold_right
              (fun param ty -> Forall (param, ty))
              params
              (Fun
                 ( [ ty ],
                   TyConstructor (data_name, List.map (fun x -> TyVar x) params)
                 ))
          in
          (* Sources should *currently* always be empty since data constructors cannot contain
             constraints (We are not going to copy that mistake from old Haskell).
             However, this might change if we ever get GADTs so let's make sure this already
             handles that case correctly *)
          let instantiated, targets =
            instantiate loc env data_constructor_type
          in
          ( instantiated,
            apply_targets loc targets
              (Typed.DataConstructor ((loc, data_constructor_type), data_name))
          )
      | None ->
          panic __LOC__
            ("Unbound data constructor in type checker: '"
           ^ Name.pretty data_name ^ "'")
    end
  | ExceptionConstructor (loc, exception_name) -> begin
      match NameMap.find_opt exception_name env.exception_definitions with
      | Some param_tys ->
          ( Fun (param_tys, Exception),
            ExceptionConstructor (loc, exception_name) )
      | None ->
          panic __LOC__
            ("Unbound exception constructor in type checker: '"
           ^ Name.pretty exception_name ^ "'")
    end
  | VariantConstructor (loc, constructor_name, args) ->
      (* We infer variant constructors to an open variant type.
         The reasoning behind this is that a constructor A(..) can be used as part of
         any set of constructors as long as that set contains A(..), so its type should be
         < A(..) | ?r > *)
      let arg_types, args = List.split (List.map (infer env) args) in
      let ext_field = fresh_unif_raw env in
      ( VariantUnif ([| (constructor_name, arg_types) |], ext_field),
        VariantConstructor (loc, constructor_name, args) )
  | ModSubscriptDataCon (void, _, _, _) -> .
  | App (loc, fun_expr, args) ->
      (* We infer the function type and then match the arguments against that.
         This is both more efficient than inferring the arguments first, since most functions
         are just going to be variables with known types, and necessary to check
         higher rank types, since arguments can be checked against a polytype, but not inferred. *)
      let fun_ty, fun_expr = infer env fun_expr in
      let param_tys, result_ty =
        split_fun_ty env loc (List.length args) fun_ty
      in

      let args =
        match Base.List.map2 param_tys args ~f:(check env) with
        | Ok args -> args
        | Unequal_lengths ->
            raise
              (TypeError
                 ( loc,
                   PassedIncorrectNumberOfArgsToFun
                     (List.length args, param_tys, result_ty) ))
      in
      (result_ty, App (loc, fun_expr, args))
  | Lambda (loc, args, body) ->
      let arg_tys, transformers, args =
        Util.split3 (List.map (fun p -> infer_pattern env true p) args)
      in
      let env = Util.compose transformers env in
      let result_ty, body = infer env body in
      (Fun (arg_tys, result_ty), Lambda (loc, args, body))
  | StringLit (loc, literal) -> (String, StringLit (loc, literal))
  | NumLit (loc, literal) -> (Number, NumLit (loc, literal))
  | BoolLit (loc, literal) -> (Bool, BoolLit (loc, literal))
  | UnitLit loc -> (Ty.unit, UnitLit loc)
  | ListLit (loc, []) ->
      let elem_ty = fresh_unif env in
      (List elem_ty, ListLit (loc, []))
  | ListLit (loc, expr :: exprs) ->
      let elem_ty, expr = infer env expr in
      let exprs = List.map (check env elem_ty) exprs in
      (List elem_ty, ListLit (loc, expr :: exprs))
  | TupleLit (loc, exprs) ->
      let elem_types, exprs = List.split (List.map (infer env) exprs) in
      (Tuple (Array.of_list elem_types), TupleLit (loc, exprs))
  | RecordLit (loc, fields) ->
      let typed_fields =
        Array.map (fun (x, expr) -> (x, infer env expr)) (Array.of_list fields)
      in
      ( RecordClosed (Array.map (fun (x, (ty, _)) -> (x, ty)) typed_fields),
        RecordLit
          ( loc,
            Array.to_list
              (Array.map (fun (x, (_, expr)) -> (x, expr)) typed_fields) ) )
  | StringInterpolation (loc, components) ->
      let components =
        List.map (check_interpolation_component env) components
      in
      (String, StringInterpolation (loc, components))
  | Subscript (loc, expr, name) ->
      let val_ty = fresh_unif env in
      let u, u_name = fresh_unif_raw env in
      let expr =
        check env (RecordUnif ([| (name, val_ty) |], (u, u_name))) expr
      in
      (val_ty, Subscript ((loc, val_ty), expr, name))
  | ModSubscript (loc, mod_name, key_name) -> begin
      match NameMap.find_opt mod_name env.module_var_contents with
      | None ->
          panic __LOC__
            ("Module not found while typechecking: '" ^ Name.pretty mod_name
           ^ "'. This should have been caught earlier!")
      | Some mod_env -> begin
          match NameMap.find_opt key_name mod_env.var_types with
          | None ->
              panic __LOC__
                ("Module does not contain variable: '" ^ Name.pretty key_name
               ^ "'. This should have been caught earlier!")
          | Some ty ->
              let instantiated, targets = instantiate loc env ty in
              ( instantiated,
                apply_targets loc targets
                  (ModSubscript ((loc, ty), mod_name, key_name)) )
        end
    end
  | RecordUpdate (loc, expr, field_updates) ->
      let update_typed_fields =
        Array.map
          (fun (x, expr) -> (x, infer env expr))
          (Array.of_list field_updates)
      in
      let unif_raw = fresh_unif_raw env in

      let record_ty =
        RecordUnif
          (Array.map (fun (x, (ty, _)) -> (x, ty)) update_typed_fields, unif_raw)
      in
      let expr = check env record_ty expr in
      ( record_ty,
        RecordUpdate
          ( loc,
            expr,
            Array.to_list
              (Array.map (fun (x, (_, expr)) -> (x, expr)) update_typed_fields)
          ) )
  | RecordExtension (loc, expr, field_exts) ->
      (* TODO: At the moment, we infer record extensions by checking the underlying field
         against a unification variable and then returning a unification record with the same variable
         as its extension field. It might be more efficient to infer the underlying type properly and
         then merge directly *)
      let field_tys =
        Array.map
          (fun (x, expr) -> (x, infer env expr))
          (Array.of_list field_exts)
      in
      let u, u_name = fresh_unif_raw env in
      let expr = check env (Unif (u, u_name)) expr in
      ( RecordUnif
          (Array.map (fun (x, (ty, _)) -> (x, ty)) field_tys, (u, u_name)),
        RecordExtension
          ( loc,
            expr,
            Array.to_list
              (Array.map (fun (x, (_, expr)) -> (x, expr)) field_tys) ) )
  | DynLookup (loc, list_expr, index_expr) ->
      let element_type = fresh_unif env in
      let list_expr = check env (List element_type) list_expr in
      let index_expr = check env Number index_expr in
      (element_type, DynLookup (loc, list_expr, index_expr))
  | BinOp (loc, expr1, ((Add | Sub | Mul | Div) as op), expr2) ->
      let expr1 = check env Number expr1 in
      let expr2 = check env Number expr2 in
      (Number, BinOp (loc, expr1, coerce_bin_op op, expr2))
  | BinOp (loc, expr1, Concat, expr2) ->
      (* TODO: Generalize this to use type classes once they are implemented.
         Ideally, concat expressions should really just desugar to a type class method *)
      let expr1 = check env String expr1 in
      let expr2 = check env String expr2 in
      (String, BinOp (loc, expr1, Concat, expr2))
  | BinOp (loc, expr1, Cons, expr2) ->
      let ty1, expr1 = infer env expr1 in
      let expr2 = check env (List ty1) expr2 in
      (List ty1, BinOp (loc, expr1, Cons, expr2))
  | BinOp (loc, expr1, ((Equals | NotEquals) as op), expr2) ->
      let ty1, expr1 = infer env expr1 in
      let expr2 = check env ty1 expr2 in
      (Bool, BinOp (loc, expr1, coerce_bin_op op, expr2))
  | BinOp (loc, expr1, ((LE | GE | LT | GT) as op), expr2) ->
      let expr1 = check env Number expr1 in
      let expr2 = check env Number expr2 in
      (Bool, BinOp (loc, expr1, coerce_bin_op op, expr2))
  | BinOp (loc, expr1, ((Or | And) as op), expr2) ->
      let expr1 = check env Bool expr1 in
      let expr2 = check env Bool expr2 in
      (Bool, BinOp (loc, expr1, coerce_bin_op op, expr2))
  | Not (loc, expr) ->
      let expr = check env Bool expr in
      (Bool, Not (loc, expr))
  | Range (loc, first, last) ->
      let first = check env Number first in
      let last = check env Number last in
      (List Number, Range (loc, first, last))
  | ListComp (loc, result, clauses) ->
      let env, clauses = check_list_comp env clauses in
      let ty, result = infer env result in
      (List ty, ListComp (loc, result, clauses))
  | If (loc, cond, then_branch, else_branch) ->
      let cond = check env Bool cond in
      let then_ty, then_branch = infer env then_branch in

      let else_ty, else_branch = infer env else_branch in

      (* This is a bit subtle. We need to ensure that both branches have the same
         type, but we cannot just use unify, since that might fail if either branch returns
         something of a polymorphic type. Instead, we check that both types are mutual subtypes.
         This way, no type is more general than the other, so it doesn't matter which one we
         assign to the if expression as a whole. We arbitrarily pick the type of the then branch.
      *)
      let sources1, targets1 = subsumes env loc then_ty else_ty in
      let sources2, targets2 = subsumes env loc else_ty then_ty in
      (* All inferred types are ρ-types so we don't need to worry about evidence here *)
      assert (sources1 = []);
      assert (sources2 = []);
      assert (targets1 = []);
      assert (targets2 = []);

      (then_ty, If (loc, cond, then_branch, else_branch))
  | Seq (loc, exprs) ->
      let ty, exprs = infer_seq env exprs in
      (ty, Seq (loc, exprs))
  | LetSeq _
  | LetRecSeq _
  | LetEnvSeq _
  | LetModuleSeq _
  | LetDataSeq _
  | LetTypeSeq _
  | LetClassSeq _
  | LetInstanceSeq _
  | LetExceptionSeq _ ->
      panic __LOC__
        "Found LetSeq expression outside of expression block during \
         typechecking"
  | ProgCall (loc, program, args) ->
      let args_with_types = List.map (infer env) args in
      (* We require a ProgramArgument constraint on every argument for now.
         In the future, this is going to be replaced by a type class,
         which should also be more robust. *)
      List.iter
        (fun (arg_type, arg) ->
          prog_arg_constraint env (Typed.get_loc arg) arg_type)
        args_with_types;

      (String, ProgCall (loc, program, List.map snd args_with_types))
  | Pipe (loc, exprs) ->
      let rec check_progcalls = function
        | [] -> []
        | (ProgCall _ as expr) :: exprs ->
            let _, expr = infer env expr in
            let exprs = check_progcalls exprs in
            expr :: exprs
        | expr :: _ -> raise (TypeError (loc, NonProgCallInPipe expr))
      in
      let expr =
        match exprs with
        | [] -> panic __LOC__ "Empty pipe expression"
        | ProgCall _ :: _ ->
            let exprs = check_progcalls exprs in
            Typed.Pipe (loc, exprs)
        | expr :: exprs ->
            (* TODO: Use a toString typeclass *)
            let expr = check env String expr in
            let exprs = check_progcalls exprs in
            Pipe (loc, expr :: exprs)
      in
      (String, expr)
  | EnvVar (loc, name) -> (String, EnvVar (loc, name))
  | Async (loc, expr) ->
      let expr_ty, expr = infer env expr in
      (Promise expr_ty, Async (loc, expr))
  | Await (loc, expr) ->
      let expr_ty = fresh_unif env in
      let expr = check env (Promise expr_ty) expr in
      (expr_ty, Await (loc, expr))
  | Match (loc, scrutinee, branches) ->
      (* TODO: Do exhaustiveness checking. *)
      let scrutinee_type, scrutinee = infer env scrutinee in
      let result_ty = fresh_unif env in

      let branches =
        check_match_patterns env result_ty scrutinee_type branches
      in
      (result_ty, Match (loc, scrutinee, branches))
  | Ascription (loc, expr, ty) ->
      let expr = check env ty expr in
      (ty, Ascription (loc, expr, ty))
  | Unwrap (loc, expr) ->
      let ty, expr = infer env expr in
      let result_ty = fresh_unif env in
      unwrap_constraint env loc ty result_ty;
      (result_ty, Unwrap (loc, expr))
  | MakeRef (loc, expr) ->
      let ty, expr = infer env expr in
      (Ref ty, MakeRef (loc, expr))
  | Assign (loc, ref_expr, expr) ->
      (* We infer and split the type of the reference and then check the expression against that.
         We could do this in reverse, which would remove the need to split and possibly generate
         fewer constraints, but this might result in drastically worse error messages. *)
      let ref_type, ref_expr = infer env ref_expr in
      let ref_value_type = split_ref_ty env loc ref_type in

      let expr = check env ref_value_type expr in
      (Ty.unit, Assign (loc, ref_expr, expr))
  | Try (loc, try_expr, handlers) ->
      let result_type, try_expr = infer env try_expr in

      let check_handler (pattern, body) =
        let env_trans, pattern = check_pattern env true pattern Exception in
        let body = check (env_trans env) result_type body in
        (pattern, body)
      in

      let handlers = List.map check_handler handlers in

      (result_type, Try (loc, try_expr, handlers))
  | Raise (loc, expr) ->
      let expr = check env Exception expr in
      let result_type = fresh_unif env in
      (result_type, Raise (loc, expr))
  | ExprExt (_, void) -> .

and check : local_env -> ty -> expr -> Typed.expr =
 fun env polymorphic_expected_ty expr ->
  trace_tc
    (lazy
      (level_prefix env ^ "checking expression '" ^ pretty expr ^ "' : "
      ^ pretty_type polymorphic_expected_ty));
  let expected_ty, _, env_trans, sources =
    skolemize_with_function (get_loc expr) env polymorphic_expected_ty
  in

  let env = env_trans env in

  let defer_to_inference () =
    let ty, expr = infer env expr in
    let sources, targets = subsumes env (Typed.get_loc expr) ty expected_ty in
    (* infer infers a monotype, so targets shoulda always be empty here *)
    assert (targets = []);
    abstract_sources (Typed.get_loc expr) sources expr
  in

  abstract_sources (get_loc expr) sources
    begin
      match (expr, expected_ty) with
      | Var _, _ -> defer_to_inference ()
      | DataConstructor _, _ -> defer_to_inference ()
      | ExceptionConstructor _, _ -> defer_to_inference ()
      (* Variant constructors would be a bit complicated to check directly
         and we wouldn't gain much, so we just defer to inference. *)
      | VariantConstructor _, _ -> defer_to_inference ()
      | ModSubscriptDataCon (void, _, _, _), _ -> .
      | App _, _ -> defer_to_inference ()
      | Lambda (loc, param_patterns, body), expected_ty ->
          let param_tys, result_ty =
            split_fun_ty env loc (List.length param_patterns) expected_ty
          in

          let transformers, param_patterns =
            match
              Base.List.map2 param_patterns param_tys
                ~f:(check_pattern env true)
            with
            | Ok typed_pats -> List.split typed_pats
            | Unequal_lengths ->
                raise
                  (TypeError
                     ( loc,
                       IncorrectNumberOfArgsInLambda
                         (List.length param_patterns, param_tys, result_ty) ))
          in
          let env_transformer = Util.compose transformers in
          let body = check (env_transformer env) result_ty body in
          Lambda (loc, param_patterns, body)
      | StringLit (loc, literal), String -> StringLit (loc, literal)
      | NumLit (loc, literal), Number -> NumLit (loc, literal)
      | UnitLit loc, RecordClosed [||] -> UnitLit loc
      | ListLit (loc, elems), List elem_ty ->
          let elems = List.map (check env elem_ty) elems in
          ListLit (loc, elems)
      | TupleLit (loc, elems), Tuple elem_tys ->
          let elems =
            match
              Base.List.map2 (Array.to_list elem_tys) elems ~f:(check env)
            with
            | Ok elems -> elems
            | Unequal_lengths ->
                raise
                  (TypeError
                     ( loc,
                       TupleLiteralOfWrongLength (List.length elems, elem_tys)
                     ))
          in
          TupleLit (loc, elems)
          (* Record literals would be hard to check directly since the fields are order independent,
             so we just defer to inference and let the unifier deal with this. *)
      | RecordLit _, _ -> defer_to_inference ()
      (* Right now, this case is necessary to match type aliases for literal types (e.g. String).
         We could do this manually, but we still need this, since with typeclasses
         there might be actual subtypes for String! (e.g. `forall a. C a => a` where String implements C)

         We could check these directly with `subsumes`, but `defer_to_inference` will do this for us and also works for
         cases with parameters *)
      | ( ( StringLit _ | NumLit _ | BoolLit _ | UnitLit _ | ListLit _
          | TupleLit _ ),
          _ ) ->
          defer_to_inference ()
      | StringInterpolation _, _ -> defer_to_inference ()
      | Subscript (loc, expr, field), expected_ty ->
          let ext_field = fresh_unif_raw env in
          let record_ty = RecordUnif ([| (field, expected_ty) |], ext_field) in
          let expr = check env record_ty expr in
          Subscript ((loc, expected_ty), expr, field)
      | ModSubscript _, _ -> defer_to_inference ()
      (* TODO: I'm not sure if we can do this more intelligently / efficiently in check mode *)
      | RecordUpdate _, _ -> defer_to_inference ()
      (* TODO: This can probably be done more efficiently *)
      | RecordExtension _, _ -> defer_to_inference ()
      | DynLookup (loc, list_expr, index_expr), element_type ->
          let list_expr = check env (List element_type) list_expr in
          let index_expr = check env Number index_expr in
          DynLookup (loc, list_expr, index_expr)
      | BinOp (loc, expr1, Cons, expr2), List elem_ty ->
          let expr1 = check env elem_ty expr1 in
          let expr2 = check env (List elem_ty) expr2 in
          BinOp (loc, expr1, Cons, expr2)
      | BinOp _, _ -> defer_to_inference ()
      | Not (loc, expr), Bool ->
          let expr = check env Bool expr in
          Not (loc, expr)
      | Not _, _ -> defer_to_inference ()
      | Range (loc, first, last), List Number ->
          let first = check env Number first in
          let last = check env Number last in
          Range (loc, first, last)
      | Range _, _ -> defer_to_inference ()
      | ListComp (loc, result, clauses), List elem_ty ->
          let env, clauses = check_list_comp env clauses in
          let result = check env elem_ty result in
          ListComp (loc, result, clauses)
      | ListComp _, _ -> defer_to_inference ()
      | If (loc, cond, then_branch, else_branch), expected_ty ->
          let cond = check env Bool cond in
          let then_branch = check env expected_ty then_branch in
          let else_branch = check env expected_ty else_branch in
          If (loc, cond, then_branch, else_branch)
      | Seq (loc, exprs), expected_ty ->
          let exprs = check_seq env loc expected_ty exprs in
          Seq (loc, exprs)
      | ( ( LetSeq _ | LetRecSeq _ | LetEnvSeq _ | LetModuleSeq _ | LetDataSeq _
          | LetTypeSeq _ | LetExceptionSeq _ | LetClassSeq _ | LetInstanceSeq _
            ),
          _ ) ->
          panic __LOC__
            "Found LetSeq expression outside of expression block during \
             typechecking"
      | ProgCall _, _ -> defer_to_inference ()
      | Pipe _, _ -> defer_to_inference ()
      | EnvVar _, _ -> defer_to_inference ()
      | Async (loc, expr), Promise elem_ty ->
          let expr = check env elem_ty expr in
          Async (loc, expr)
      | Async _, _ -> defer_to_inference ()
      | Await (loc, expr), expected_ty ->
          let expr = check env (Promise expected_ty) expr in
          Await (loc, expr)
      | Match (loc, scrutinee, branches), expected_ty ->
          let scrutinee_type, scrutinee = infer env scrutinee in

          let branches =
            check_match_patterns env expected_ty scrutinee_type branches
          in
          Match (loc, scrutinee, branches)
      | Ascription (loc, expr, ty), expected_ty ->
          let expr = check env ty expr in
          let sources, targets = subsumes env loc ty expected_ty in
          assert (sources = []);
          Ascription (loc, apply_targets loc targets expr, ty)
      | Unwrap (loc, expr), expected_ty ->
          let ty, expr = infer env expr in
          unwrap_constraint env loc ty expected_ty;
          Unwrap (loc, expr)
      | MakeRef (loc, expr), Ref inner_type ->
          let expr = check env inner_type expr in
          MakeRef (loc, expr)
      | MakeRef _, _ -> defer_to_inference ()
      | Assign (loc, ref_expr, expr), Ref inner_type ->
          let ref_expr = check env (Ref inner_type) ref_expr in
          let expr = check env inner_type expr in
          Assign (loc, ref_expr, expr)
      | Assign _, _ -> defer_to_inference ()
      | Try (loc, try_expr, handlers), expected_ty ->
          let try_expr = check env expected_ty try_expr in

          let check_handler (pattern, body) =
            let env_trans, pattern = check_pattern env true pattern Exception in
            let body = check (env_trans env) expected_ty body in

            (pattern, body)
          in

          let handlers = List.map check_handler handlers in

          Try (loc, try_expr, handlers)
      (* Raise returns something of type forall a. a, so we can safely ignore the type it is checked against.
         This also makes it possible to check a raise expression against a polytype without impredicativity. *)
      | Raise (loc, expr), _ ->
          let expr = check env Exception expr in
          Raise (loc, expr)
      | ExprExt (_, void), _ -> .
    end

and abstract_sources :
    loc -> Evidence.source list list -> Typed.expr -> Typed.expr =
 fun loc sources expr ->
  List.fold_right
    (fun sources expr -> Typed.ExprExt (loc, DictLambda (sources, expr)))
    sources expr

and apply_targets : loc -> Evidence.target list list -> Typed.expr -> Typed.expr
    =
 fun loc targets (expr : Typed.expr) ->
  List.fold_left
    (fun expr targets -> Typed.ExprExt (loc, DictApp (expr, targets)))
    expr targets

and check_list_comp :
    local_env ->
    list_comp_clause list ->
    local_env * Typed.list_comp_clause list =
 fun env -> function
  | [] -> (env, [])
  | FilterClause expr :: clauses ->
      let expr = check env Bool expr in
      let env, clauses = check_list_comp env clauses in
      (env, FilterClause expr :: clauses)
  | DrawClause (pattern, expr) :: clauses ->
      let ty, env_trans, pattern = infer_pattern env true pattern in
      (* We use the *unaltered* environment, since
         draw clauses are non-recursive *)
      let expr = check env (List ty) expr in
      let env, clauses = check_list_comp (env_trans env) clauses in
      (env, DrawClause (pattern, expr) :: clauses)

and check_interpolation_component env = function
  | StringComponent (loc, str) -> StringComponent (loc, str)
  | Interpolation (loc, exprs) ->
      let interpolation_type, exprs = infer_seq env exprs in

      interpolatable_constraint env loc interpolation_type;

      Interpolation (loc, exprs)

and check_seq_expr :
    local_env ->
    [ `Check | `Infer ] ->
    expr ->
    (local_env -> local_env) * Typed.expr =
 fun env check_or_infer expr ->
  match expr with
  | LetSeq (loc, pat, body) ->
      let generalizable = is_value body in
      let ty, env_trans, pat = infer_pattern env generalizable pat in
      let ty, skolemizer, skolem_env_trans, expr_trans =
        skolemize_with_function loc env ty
      in
      let env = skolem_env_trans env in

      let traversal =
        object (self)
          inherit [unit] Traversal.traversal
          method! ty () ty = (skolemizer ty, ())

          (* `skolemizer` is already recursive, so we skip any unnecessary (quadratic!) traversals *)
          method! traverse_type state ty = self#ty state ty
        end
      in

      let body, () = traversal#traverse_expr () body in

      let body =
        enter_level env
          begin
            fun env -> check env ty body
          end
      in

      let env_trans =
        if generalizable then env_trans else increase_ambient_level << env_trans
      in
      (env_trans, LetSeq (loc, pat, body))
  | LetRecSeq (loc, mty, fun_name, patterns, body) ->
      let ( arg_tys,
            transformers,
            patterns,
            result_ty,
            ty_skolemizer,
            env_trans,
            sources ) =
        match Option.map (skolemize_with_function loc.main env) mty with
        | None ->
            let arg_tys, transformers, patterns =
              Util.split3 (List.map (infer_pattern env true) patterns)
            in
            (arg_tys, transformers, patterns, fresh_unif env, Fun.id, Fun.id, [])
        | Some
            (Fun (arg_tys, result_ty), ty_skolemizer, env_transformer, sources)
          ->
            let transformers, patterns =
              match
                Base.List.map2 patterns arg_tys
                  ~f:(check_pattern (env_transformer env) true)
              with
              | Ok transformers_and_patterns ->
                  List.split transformers_and_patterns
              | Unequal_lengths ->
                  raise
                    (TypeError
                       ( loc.main,
                         ArgCountMismatchInDefinition
                           (fun_name, arg_tys, List.length patterns) ))
            in
            ( arg_tys,
              transformers,
              patterns,
              result_ty,
              ty_skolemizer,
              env_transformer,
              sources )
        | Some (ty, _, _, _) ->
            raise (TypeError (loc.main, NonFunTypeInLetRec (fun_name, ty)))
      in
      let env = env_trans env in

      trace_tc
        (lazy
          ("[Infer (LetRec(Seq) ..)]: " ^ Name.pretty fun_name ^ " : "
          ^ pretty_type (Fun (arg_tys, result_ty))));

      let fun_ty =
        match mty with
        | Some ty -> ty
        | None -> Fun (arg_tys, result_ty)
      in

      let env_trans = insert_var fun_name fun_ty in

      let inner_env = Util.compose transformers (env_trans env) in

      let skolemizer_traversal =
        object (self)
          inherit [unit] Traversal.traversal
          method! ty _ ty = (ty_skolemizer ty, ())

          (* ty_skolemizer already traverses the entire type, so this disables
             any duplicate traversals (which would lead to time quadratic in the size of the type) *)
          method! traverse_type state ty = self#ty state ty
        end
      in

      let body, () = skolemizer_traversal#traverse_expr () body in
      (* Without a type annotation, we have to check the body against a unification variable instead of inferring,
          since we need to know the functions type in its own (possibly recursive) definition*)
      let body =
        enter_level inner_env
          begin
            fun inner_env -> check inner_env result_ty body
          end
      in
      ( env_trans,
        (* TODO: I don't think this concat is correct :/ It doesn't make a difference *yet* though
           since all constraints are atomic for now *)
        LetRecSeq
          ((loc, fun_ty, List.concat sources), mty, fun_name, patterns, body) )
  | LetEnvSeq (loc, envvar, expr) ->
      (* TODO: Once typeclasses are implemented, the expr should really just have to implement
         some kind of 'ToString' typeclass. For now we require the expr to be an exact string though *)
      let expr = check env String expr in
      (Fun.id, LetEnvSeq (loc, envvar, expr))
  | LetModuleSeq (loc, name, mod_expr) ->
      let module_env, mod_expr = eval_module_env env mod_expr in
      (* Variables should *not* be merged with the current environment,
         since we still want modules to be qualified (at least by default)

         At the same time, we *do* remove module subscripts from types and data constructors
         so we *need to merge these* with the current environment.
         (This is fine since data constructor names are always unique, even across modules)
      *)
      let data_definitions_with_levels =
        NameMap.map
          (fun (params, underlying) -> (env.level, params, underlying))
          module_env.data_definitions
      in
      ( (fun env ->
          {
            env with
            module_var_contents =
              NameMap.add name module_env env.module_var_contents;
            data_definitions =
              NameMap.union
                (fun _ _ x -> Some x)
                env.data_definitions data_definitions_with_levels;
            exception_definitions =
              NameMap.union
                (fun _ _ x -> Some x)
                env.exception_definitions module_env.exception_definitions;
          }),
        LetModuleSeq (loc, name, mod_expr) )
  | LetDataSeq (loc, data_name, params, ty) ->
      ( insert_data_definition env.level data_name params ty,
        LetDataSeq (loc, data_name, params, ty) )
  | LetTypeSeq (loc, alias_name, params, ty) ->
      ( insert_type_alias alias_name params ty,
        LetTypeSeq (loc, alias_name, params, ty) )
  | LetClassSeq (loc, class_name, params, methods) ->
      let insert_method (name, ty) env =
        let class_constraint =
          TyConstructor (class_name, List.map (fun x -> TyVar x) params)
        in
        let type_with_class =
          List.fold_right
            (fun name rest -> Forall (name, rest))
            params
            (Constraint (class_constraint, ty))
        in
        insert_var name type_with_class env
      in
      let insert_methods env = List.fold_right insert_method methods env in
      ( insert_methods << insert_type_class class_name params,
        LetClassSeq (loc, class_name, params, methods) )
  | LetInstanceSeq (loc, universals, entailed, class_name, arguments, methods)
    ->
      (* TODO: Does this really need to be part of the environment? Can't we just
         annotate this with the parameters in the renamer? *)
      let parameter_names =
        match NameMap.find_opt class_name env.type_classes with
        | None ->
            panic __LOC__
              (Loc.pretty loc ^ ": Type class not found in type checker: "
             ^ Name.pretty class_name)
        | Some parameters -> parameters
      in

      let check_method (expected_type_raw, name, parameters, body) =
        let expected_type =
          replace_tvars
            (NameMap.of_seq
               (List.to_seq (List.combine parameter_names arguments)))
            expected_type_raw
        in
        let argument_types, body_type =
          split_fun_ty env loc (List.length arguments) expected_type
        in

        let env_transformers, parameters =
          List.split
            (List.map2 (check_pattern env true) parameters argument_types)
        in

        (* Methods can be recursive, but recursive calls should use the *class* method (which is already in scope),
           rather than the one in this specific instance! *)
        let inner_env = Util.compose env_transformers env in

        let body = check inner_env body_type body in

        (expected_type, name, parameters, body)
      in
      let methods = List.map check_method methods in

      let given_source = Evidence.make_source () in

      let entailed_constraints, entailed_evidence =
        collect_atomic_given_constraints entailed
      in

      let entailed_constraints = List.map fst entailed_constraints in

      (* TODO: Annotate the instance with its given source here *)
      ( emit_givens loc
          [
            ( { class_name; variables = []; arguments },
              given_source,
              entailed_constraints );
          ],
        LetInstanceSeq
          ( (loc, given_source),
            universals,
            entailed,
            class_name,
            arguments,
            methods ) )
  | LetExceptionSeq (loc, exception_name, params, message_expr) ->
      let message_env =
        List.fold_right (fun (param, ty) r -> insert_var param ty r) params env
      in
      let message_expr = check message_env String message_expr in

      ( insert_exception_definition exception_name (List.map snd params),
        LetExceptionSeq (loc, exception_name, params, message_expr) )
  | ProgCall (loc, prog, args) ->
      let args_with_types = List.map (infer env) args in
      (* See the inference rule for ProgCall expressions *)
      List.iter
        (fun (arg_type, arg) ->
          prog_arg_constraint env (Typed.get_loc arg) arg_type)
        args_with_types;

      (Fun.id, ProgCall (loc, prog, List.map snd args_with_types))
  | Pipe _ as expr ->
      (* We defer to `infer` here. We don't care about the result type, since
         a) We know it is `String`
         b) In this context, pipes don't actually return anything, but print to stdout *)
      let _, expr = infer env expr in
      (Fun.id, expr)
  | expr -> (
      match check_or_infer with
      | `Check ->
          let expr = check env Ty.unit expr in
          (Fun.id, expr)
      | `Infer ->
          let _type, expr = infer env expr in
          (Fun.id, expr))

and infer_seq : local_env -> expr list -> ty * Typed.expr list =
 fun env exprs ->
  match exprs with
  | [] -> (Ty.unit, [])
  | [
   (( LetSeq _ | LetRecSeq _ | LetEnvSeq _ | LetModuleSeq _ | LetDataSeq _
    | LetTypeSeq _ ) as expr);
  ] ->
      (* If the last expression in an expression block is a
         LetSeq* expresion, we don't need to carry the environment transformations,
         but we do have to make sure to check the expression with `check_seq_expr`,
         so it is not passed to `infer`
      *)
      let _, expr = check_seq_expr env `Check expr in
      (Ty.unit, [ expr ])
  | [ expr ] ->
      let ty, expr = infer env expr in
      (ty, [ expr ])
  | expr :: exprs ->
      let env_trans, expr = check_seq_expr env `Check expr in
      let env, exprs = infer_seq (env_trans env) exprs in
      (env, expr :: exprs)

and check_seq : local_env -> loc -> ty -> expr list -> Typed.expr list =
 fun env loc expected_ty exprs ->
  match exprs with
  | [] ->
      let sources, targets = subsumes env loc Ty.unit expected_ty in
      assert (targets = []);
      begin
        match sources with
        | [] -> []
        (* If the expected type adds any evidence, we desugar to a unit literal instead
           of an empty sequence in order to apply the correct dictionary abstractions *)
        | sources -> [ abstract_sources loc sources (UnitLit loc) ]
      end
  | [
   (( LetSeq _ | LetRecSeq _ | LetEnvSeq _ | LetModuleSeq _ | LetDataSeq _
    | LetTypeSeq _ ) as expr);
  ] ->
      (* If the last expression in an expression block is a
         LetSeq* expresion, we don't need to carry the environment transformations,
         but we do have to make sure to check the expression with `check_seq_expr`,
         so it is not passed to `check`
      *)
      let _, expr = check_seq_expr env `Check expr in
      (* In tail position, these should return unit *)
      let sources, targets =
        subsumes env (Typed.get_loc expr) expected_ty Ty.unit
      in
      assert (sources = []);
      [ apply_targets loc targets expr ]
  | [ expr ] -> [ check env expected_ty expr ]
  | expr :: exprs ->
      let env_trans, expr = check_seq_expr env `Check expr in
      let exprs = check_seq (env_trans env) loc expected_ty exprs in
      expr :: exprs

and check_match_patterns env result_type scrutinee_type = function
  | [] -> []
  | (pattern, expr) :: branches ->
      let pattern_loc = get_pattern_loc pattern in
      let env_trans, pattern = check_pattern env true pattern scrutinee_type in
      let expr = check (env_trans env) result_type expr in

      let scrutinee_type =
        match Pattern.check_variant_refutability pattern with
        | None -> scrutinee_type
        | Some (path, name) ->
            refine_variant env pattern_loc scrutinee_type path name
      in

      let rest = check_match_patterns env result_type scrutinee_type branches in
      (pattern, expr) :: rest

let datacon_level : name -> local_env -> Typeref.level =
 fun constructor env ->
  match NameMap.find_opt constructor env.data_definitions with
  | None ->
      panic __LOC__
        ("Trying to look up level of unbound data constructor "
       ^ Name.pretty constructor)
  | Some (level, _, _) -> level

(* Perform an occurs check and adjust levels (See Note [Levels]) *)
let occurs_and_adjust needle name full_ty loc definition_env
    optional_unify_context =
  let traversal =
    object
      inherit [bool] Traversal.traversal

      method! ty state ty =
        match normalize_unif ty with
        | Unif (typeref, _) as ty when Typeref.equal needle typeref -> (ty, true)
        | Unif (typeref, name) as ty -> begin
            match Typeref.get needle with
            | Bound _ ->
                panic __LOC__
                  ("Trying to perform an occurs check on a bound unification \
                    variable: "
                  ^ pretty_type (Unif (needle, Name.fresh "unknown")))
            | Unbound needle_level ->
                Typeref.adjust_level needle_level typeref;
                (ty, state)
          end
        | Skol (_, skol_level, _) as skol -> begin
            match Typeref.get needle with
            | Bound _ ->
                panic __LOC__
                  ("Trying to perform an occurs check on a bound unification \
                    variable: "
                  ^ pretty_type (Unif (needle, Name.fresh "unknown")))
            | Unbound needle_level ->
                if
                  Typeref.unifiable_level ~type_level:skol_level
                    ~unif_level:needle_level
                then (ty, state)
                else
                  raise
                    (TypeError
                       ( loc,
                         SkolemUnifyEscape
                           ( Unif (needle, name),
                             skol,
                             full_ty,
                             optional_unify_context ) ))
          end
        | TyConstructor (constructor_name, _) -> begin
            match Typeref.get needle with
            | Bound _ ->
                panic __LOC__
                  ("Trying to perform an occurs check on a bound unification \
                    variable: "
                  ^ pretty_type (Unif (needle, Name.fresh "unknown")))
            | Unbound needle_level ->
                if
                  Typeref.unifiable_level
                    ~type_level:(datacon_level constructor_name definition_env)
                    ~unif_level:needle_level
                then (ty, state)
                else
                  raise
                    (TypeError
                       ( loc,
                         DataConUnifyEscape
                           ( Unif (needle, name),
                             constructor_name,
                             full_ty,
                             optional_unify_context ) ))
          end
        | ty -> (ty, state)
    end
  in
  snd (traversal#traverse_type false full_ty)

type unify_state = { deferred_constraints : ty_constraint Difflist.t ref }

(** Bind a typeref to a new type
    This is like bind_directly but skips the occurs check
    and does not adjust any levels (See Note [Levels]).

    This is only safe if the substituted type does not
    contain any unification variables from an outer scope. *)
let bind_unchecked : ty Typeref.t -> name -> ty -> unit =
 fun typeref name ty ->
  trace_subst
    (lazy (pretty_type (Unif (typeref, name)) ^ " := " ^ pretty_type ty));
  match Typeref.get typeref with
  | Bound previous_ty ->
      panic __LOC__
        ("Trying to directly bind already bound type variable "
       ^ pretty_name name ^ "$"
        ^ Unique.display (Typeref.get_unique typeref)
        ^ ".\n" ^ "    previous type: " ^ pretty_type previous_ty ^ "\n"
        ^ "         new type: " ^ pretty_type ty ^ "\n")
  | Unbound _ -> Typeref.set typeref ty

(** Bind a typeref to a new type.
    This panics if the typeref has already been bound *)
let bind_directly :
    loc ->
    ty Typeref.t ->
    name ->
    ty ->
    local_env ->
    unify_context option ->
    unit =
 fun loc typeref name ty definition_env unify_context ->
  if occurs_and_adjust typeref name ty loc definition_env unify_context then
    raise (TypeError (loc, OccursCheck (typeref, name, ty, unify_context)))
  else bind_unchecked typeref name ty

let solve_unify :
    loc -> local_env -> unify_state -> ty -> ty -> local_env -> bool =
 fun loc env state original_type1 original_type2 definition_env ->
  trace_unify
    (lazy (pretty_type original_type1 ^ " ~ " ^ pretty_type original_type2));

  let unify_context = (original_type1, original_type2) in

  let rec go_with_original optional_unify_context ty1 ty2 =
    (* We define go this way so that unify_context is None on the initial call and
       Some (original_ty1, original_ty2) on every recursive call.
       This is used in error messages to only display the full unification context if it is
       actually relevant *)
    let go = go_with_original (Some unify_context) in

    let bind_with_context typeref name ty context =
      match Typeref.get typeref with
      | Unbound _ -> bind_directly loc typeref name ty definition_env context
      (* TODO: Preserve the order somehow for the sake of error messages *)
      | Bound previous_ty ->
          trace_unify
            (lazy
              ("bind: "
              ^ pretty_type (Unif (typeref, name))
              ^ " ~ " ^ pretty_type ty));
          let _ = go previous_ty ty in
          ()
    in
    (* bind assumes that an occurs check violation is only possible if the unify_context
       is relevant. If this is not the case (only when solving constraints between unification variables directly),
       bind_with_context should be used with optional_unify_context *)
    let bind typeref name ty =
      bind_with_context typeref name ty (Some unify_context)
    in

    (* `remaining_cont` is called with the remaining fields from both rows,
       that is the fields that are not part of the other row.
       Closed rows will generally error on this, but unif rows might continue.

       This takes a parameter `unify_fields` that specifies how to unify the row fields.
       For records, this should usually just be `go`, but for variants, where
       individual constructors can take multiple arguments, this should be `go_variant` which is
       essentially just `List.iter2 go` with parameter count checks
    *)
    (* TODO: Use maps instead of lists to make this more efficient.
       This might be a bit harder than it sounds, since we have to make sure to
       treat duplicate labels correctly (With the semantics described by Leijen et al) *)
    let unify_rows unify_fields fields1 fields2 remaining_cont =
      let rec go_rows remaining1 = function
        | (field1, ty1) :: fields1, fields2 -> begin
            match Util.extract (fun (x, _) -> x = field1) fields2 with
            (* If `field1` is *not* contained in `fields2`, we still have to
               keep it around for `remaining_cont` *)
            | None -> go_rows ((field1, ty1) :: remaining1) (fields1, fields2)
            | Some ((field2, ty2), fields2) ->
                unify_fields field2 ty1 ty2;
                go_rows remaining1 (fields1, fields2)
          end
        | [], remaining2 -> (
            match (remaining1, remaining2) with
            | [], [] -> ()
            | _ -> remaining_cont remaining1 remaining2)
      in
      go_rows [] (Array.to_list fields1, Array.to_list fields2)
    in
    let go_variant constructor_name params1 params2 =
      if List.compare_lengths params1 params2 <> 0 then
        raise
          (TypeError
             ( loc,
               DifferentVariantConstrArgs
                 (constructor_name, params1, params2, unify_context) ))
      else
        List.iter2
          (fun ty1 ty2 ->
            let _ = go ty1 ty2 in
            ())
          params1 params2
    in
    match (normalize_unif ty1, normalize_unif ty2) with
    | Unif (typeref, name), ty
    | ty, Unif (typeref, name) -> begin
        (* Thanks to normalize_unif, we know that these have to be unbound unification variables *)
        match ty with
        (* Ignore 'a ~ a' constraints. These are mostly harmless,
           but might hang the type checker if they become part of the substitution
        *)
        | Unif (typeref2, _) when Typeref.equal typeref typeref2 -> true
        | ty ->
            bind_with_context typeref name ty optional_unify_context;
            true
      end
    | TyConstructor (name1, args1), TyConstructor (name2, args2) ->
        if Name.compare name1 name2 <> 0 then
          raise
            (TypeError
               (loc, MismatchedTyCon (name1, name2, optional_unify_context)))
        else begin
          if List.compare_lengths args1 args2 <> 0 then
            panic __LOC__
              (Loc.pretty loc
             ^ ": Trying to unify applications of type constructor '"
             ^ Name.pretty name1
             ^ "' to different numbers of arguments.\n    ty1: "
             ^ pretty_type ty1 ^ "\n    ty2: " ^ pretty_type ty2)
          else begin
            List.exists2 go args1 args2
          end
        end
    | Fun (dom1, cod1), Fun (dom2, cod2) ->
        if List.compare_lengths dom1 dom2 != 0 then
          raise
            (TypeError
               (loc, FunctionsWithDifferentArgCounts (dom1, dom2, unify_context)))
        else begin
          let progress1 = List.exists2 go dom1 dom2 in
          let progress2 = go cod1 cod2 in
          progress1 || progress2
        end
    | Tuple tys1, Tuple tys2 when Array.length tys1 = Array.length tys2 ->
        List.exists2 go (Array.to_list tys1) (Array.to_list tys2)
    | List ty1, List ty2 -> go ty1 ty2
    | Promise ty1, Promise ty2 -> go ty1 ty2
    | Ref ty1, Ref ty2 -> go ty1 ty2
    | Forall (var1, body1), Forall (var2, body2) ->
        (* Foralls are unified through α-conversion, by replacing
           the type variables in both types with the same fresh skolem. *)
        let skolem = fresh_skolem_with env var1 in
        go (replace_tvar var1 skolem body1) (replace_tvar var2 skolem body2)
    | Forall _, _
    | _, Forall _ ->
        raise
          (TypeError (loc, Impredicative ((ty1, ty2), optional_unify_context)))
    | Number, Number
    | Bool, Bool
    | String, String
    | Exception, Exception ->
        true
    | Skol (u1, level1, _), Skol (u2, level2, _) when Unique.equal u1 u2 ->
        assert (level1 = level2);
        true
    (* closed, closed *)
    | RecordClosed fields1, RecordClosed fields2 ->
        unify_rows
          (fun _ ty1 ty2 ->
            let _ = go ty1 ty2 in
            ())
          fields1 fields2
          (fun remaining1 remaining2 ->
            raise
              (TypeError
                 ( loc,
                   MissingRecordFields (remaining1, remaining2, unify_context)
                 )));
        true
    | VariantClosed fields1, VariantClosed fields2 ->
        unify_rows go_variant fields1 fields2 (fun remaining1 remaining2 ->
            raise
              (TypeError
                 ( loc,
                   MissingVariantConstructors
                     (remaining1, remaining2, unify_context) )));
        true
    (* unif, closed *)
    | RecordUnif (fields1, (u, name)), RecordClosed fields2 ->
        unify_rows
          (fun _ ty1 ty2 ->
            let _ = go ty1 ty2 in
            ())
          fields1 fields2
          begin
            fun remaining1 remaining2 ->
              match remaining1 with
              | [] -> bind u name (RecordClosed (Array.of_list remaining2))
              | _ ->
                  raise
                    (TypeError
                       (loc, MissingRecordFields (remaining1, [], unify_context)))
          end;
        true
    | VariantUnif (fields1, (typeref, name)), VariantClosed fields2 ->
        unify_rows go_variant fields1 fields2
          begin
            fun remaining1 remaining2 ->
              match remaining1 with
              | [] ->
                  bind typeref name (VariantClosed (Array.of_list remaining2))
              | _ ->
                  raise
                    (TypeError
                       ( loc,
                         MissingVariantConstructors
                           (remaining1, [], unify_context) ))
          end;
        true
    (* closed, unif *)
    | RecordClosed fields1, RecordUnif (fields2, (u, name)) ->
        unify_rows
          (fun _ ty1 ty2 ->
            let _ = go ty1 ty2 in
            ())
          fields1 fields2
          begin
            fun remaining1 remaining2 ->
              match remaining2 with
              | [] -> bind u name (RecordClosed (Array.of_list remaining1))
              | _ ->
                  raise
                    (TypeError
                       (loc, MissingRecordFields ([], remaining2, unify_context)))
          end;
        true
    | VariantClosed fields1, VariantUnif (fields2, (u, name)) ->
        unify_rows go_variant fields1 fields2
          begin
            fun remaining1 remaining2 ->
              match remaining2 with
              | [] -> bind u name (VariantClosed (Array.of_list remaining1))
              | _ ->
                  raise
                    (TypeError
                       ( loc,
                         MissingVariantConstructors
                           ([], remaining2, unify_context) ))
          end;
        true
    (* unif, unif *)
    | RecordUnif (fields1, (u1, name1)), RecordUnif (fields2, (u2, name2)) ->
        unify_rows
          (fun _ ty1 ty2 ->
            let _ = go ty1 ty2 in
            ())
          fields1 fields2
          begin
            fun remaining1 remaining2 ->
              if Typeref.equal u1 u2 then
                (* TODO: Maybe this should have a more specific error message? *)
                raise
                  (TypeError
                     ( loc,
                       MissingRecordFields
                         (remaining1, remaining2, unify_context) ))
              else begin
                let new_u, new_name = fresh_unif_raw_with env "µ" in
                bind u1 name1
                  (RecordUnif (Array.of_list remaining2, (new_u, new_name)));
                bind u2 name2
                  (RecordUnif (Array.of_list remaining1, (new_u, new_name)))
              end
          end;
        true
    | VariantUnif (fields1, (u1, name1)), VariantUnif (fields2, (u2, name2)) ->
        unify_rows go_variant fields1 fields2
          begin
            fun remaining1 remaining2 ->
              if Typeref.equal u1 u2 then
                (* TODO: Maybe this should have a more specific error message? *)
                raise
                  (TypeError
                     ( loc,
                       MissingVariantConstructors
                         (remaining1, remaining2, unify_context) ))
              else begin
                let new_u, new_name = fresh_unif_raw_with env "µ" in

                bind u1 name1
                  (VariantUnif (Array.of_list remaining2, (new_u, new_name)));
                bind u2 name2
                  (VariantUnif (Array.of_list remaining1, (new_u, new_name)))
              end
          end;
        true
    (* unif, skolem *)
    (* This is almost exactly like the (unif, closed) case, except that we need to carry the
       skolem extension field over *)
    | ( RecordUnif (fields1, (unif_unique, unif_name)),
        RecordSkol (fields2, (skol_unique, skol_level, skol_name)) ) ->
        (* TODO: Only unify if levels are correct *)
        unify_rows
          (fun _ ty1 ty2 ->
            let _ = go ty1 ty2 in
            ())
          fields1 fields2
          begin
            fun remaining1 remaining2 ->
              match remaining1 with
              | [] ->
                  bind unif_unique unif_name
                    (RecordSkol
                       ( Array.of_list remaining2,
                         (skol_unique, skol_level, skol_name) ))
              | _ ->
                  raise
                    (TypeError
                       (loc, MissingRecordFields (remaining1, [], unify_context)))
          end;
        true
    | ( VariantUnif (fields1, (unif_unique, unif_name)),
        VariantSkol (fields2, (skol_unique, skol_level, skol_name)) ) ->
        (* TODO: Only unify if levels are correct *)
        unify_rows go_variant fields1 fields2
          begin
            fun remaining1 remaining2 ->
              match remaining1 with
              | [] ->
                  bind unif_unique unif_name
                    (VariantSkol
                       ( Array.of_list remaining2,
                         (skol_unique, skol_level, skol_name) ))
              | _ ->
                  raise
                    (TypeError
                       ( loc,
                         MissingVariantConstructors
                           (remaining1, [], unify_context) ))
          end;
        true
    (* skolem, unif *)
    (* This is almost exactly like the (closed, unif) case, except that we need to carry the
       skolem extension field over *)
    | ( RecordSkol (fields1, (skolem_unique, skol_level, skolem_name)),
        RecordUnif (fields2, (unif_unique, unif_name)) ) ->
        (* TODO: Only unify if levels are correct *)
        unify_rows
          (fun _ ty1 ty2 ->
            let _ = go ty1 ty2 in
            ())
          fields1 fields2
          begin
            fun remaining1 remaining2 ->
              match remaining2 with
              | [] ->
                  bind unif_unique unif_name
                    (RecordSkol
                       ( Array.of_list remaining1,
                         (skolem_unique, skol_level, skolem_name) ))
              | _ ->
                  raise
                    (TypeError
                       (loc, MissingRecordFields ([], remaining2, unify_context)))
          end;
        true
    | ( VariantSkol (fields1, (skolem_unique, skolem_level, skolem_name)),
        VariantUnif (fields2, (unif_unique, unif_name)) ) ->
        (* TODO: Only unify if levels are correct *)
        unify_rows go_variant fields1 fields2
          begin
            fun remaining1 remaining2 ->
              match remaining2 with
              | [] ->
                  bind unif_unique unif_name
                    (VariantSkol
                       ( Array.of_list remaining1,
                         (skolem_unique, skolem_level, skolem_name) ))
              | _ ->
                  raise
                    (TypeError
                       ( loc,
                         MissingVariantConstructors
                           ([], remaining2, unify_context) ))
          end;
        true
    (* skolem, skolem *)
    (* Skolem rows only unify if the skolem fields match and
        all fields unify (similar to closed rows) *)
    | ( RecordSkol (fields1, (skolem1_unique, skolem1_level, skolem1_name)),
        RecordSkol (fields2, (skolem2_unique, skolem2_level, skolem2_name)) ) ->
        (* TODO: Only unify if levels are correct *)
        (* We unify the skolems to generate a more readable error message.
           TODO: Maybe a custom error message is clearer? *)
        let _ =
          go
            (Skol (skolem1_unique, skolem1_level, skolem1_name))
            (Skol (skolem2_unique, skolem2_level, skolem2_name))
        in
        unify_rows
          (fun _ ty1 ty2 ->
            let _ = go ty1 ty2 in
            ())
          fields1 fields2
          (fun remaining1 remaining2 ->
            raise
              (TypeError
                 ( loc,
                   MissingRecordFields (remaining1, remaining2, unify_context)
                 )));
        true
    | ( VariantSkol (fields1, (skolem1_unique, skolem1_level, skolem1_name)),
        VariantSkol (fields2, (skolem2_unique, skolem2_level, skolem2_name)) )
      ->
        (* TODO: Only unify if levels are correct *)
        (* We unify the skolems to generate a more readable error message.
           TODO: Maybe a custom error message is clearer? *)
        let _ =
          go
            (Skol (skolem1_unique, skolem1_level, skolem1_name))
            (Skol (skolem2_unique, skolem2_level, skolem2_name))
        in
        unify_rows go_variant fields1 fields2 (fun remaining1 remaining2 ->
            raise
              (TypeError
                 ( loc,
                   MissingVariantConstructors
                     (remaining1, remaining2, unify_context) )));
        true
        (* closed, skolem *)
        (* skolem, closed *)
        (* Unifying a skolem and closed record is always impossible so we don't need a dedicated case here. *)
    | RecordVar _, _
    | _, RecordVar _
    | VariantVar _, _
    | _, VariantVar _ ->
        panic __LOC__
          (Loc.pretty loc
         ^ ": Uninstantiated row variable found during unification")
    | TyVar _, _
    | _, TyVar _ ->
        panic __LOC__
          (Loc.pretty loc
         ^ ": Uninstantiated type variable found during unification")
    | TypeAlias { underlying; _ }, other_type ->
        (* TODO: Remember the alias here (if possible) for error messages *)
        go underlying other_type
    | other_type, TypeAlias { underlying; _ } -> go other_type underlying
    | _ ->
        raise
          (TypeError (loc, UnableToUnify ((ty1, ty2), optional_unify_context)))
  in
  go_with_original None original_type1 original_type2

let solve_unwrap :
    loc -> local_env -> unify_state -> ty -> ty -> local_env -> bool =
 fun loc env state ty1 ty2 definition_env ->
  match normalize_unif ty1 with
  | TyConstructor (name, args) ->
      let data_level, var_names, underlying_type_raw =
        begin
          match NameMap.find_opt name definition_env.data_definitions with
          | None ->
              panic __LOC__
                (Loc.pretty loc ^ ": Data constructor '" ^ Name.pretty name
               ^ "' not found in unwrap expression. This should have been \
                  caught earlier!")
          | Some (data_level, var_names, underlying_type_raw) ->
              (data_level, var_names, underlying_type_raw)
        end
      in
      (* TODO: Can local types escape this way? *)
      let underlying_type =
        replace_tvars
          (NameMap.of_seq (Seq.zip (List.to_seq var_names) (List.to_seq args)))
          underlying_type_raw
      in
      let _ = solve_unify loc env state underlying_type ty2 definition_env in
      true
  | Ref value_type ->
      let _ = solve_unify loc env state value_type ty2 definition_env in
      true
  | (Skol _ | Unif _) as ty ->
      (* Re-queue this constraint since it might become solvable later *)
      state.deferred_constraints :=
        Difflist.snoc
          !(state.deferred_constraints)
          (Unwrap (loc, ty, ty2, definition_env));
      false
  | ty ->
      (* We know this constraint is unsolvable so we can just throw a type error *)
      raise (TypeError (loc, CannotUnwrapNonData ty))

let rec solve_program_arg :
    loc -> local_env -> unify_state -> ty -> local_env -> bool =
 fun loc env state ty definition_env ->
  match normalize_unif ty with
  | String
  | Number ->
      true
  | List ty -> solve_program_arg loc env state ty definition_env
  | (Unif _ | Skol _) as ty ->
      (* This constraint might still be solved in the future so we add it to the queue and try again *)
      state.deferred_constraints :=
        Difflist.snoc
          !(state.deferred_constraints)
          (ProgramArg (loc, ty, definition_env));
      false
  | ty ->
      (* We know the constraint is unsolvable so we don't need to try solving it again *)
      (* TODO: Collect a list of errors *)
      raise (TypeError (loc, NonProgramArgument ty))

let solve_refine_variant loc env unify_state ty path variant result_type
    definition_env =
  let remove_variant constructors =
    Array.of_list (List.remove_assoc variant (Array.to_list constructors))
  in

  let rec go path ty =
    let map_variant name index path constructors =
      let mapped =
        Util.map_array_once
          (fun (variant, tys) ->
            if String.equal name variant then begin
              let tys = Util.map_at index (go path) tys in
              Some (variant, tys)
            end
            else None)
          constructors
      in
      begin
        match mapped with
        | None ->
            panic __LOC__
              ("Non-tuple at tuple path segment: " ^ Typed.pretty_type ty)
        | Some mapped -> mapped
      end
    in
    match (path, normalize_unif ty) with
    | [], VariantClosed constructors ->
        VariantClosed (remove_variant constructors)
    | [], VariantUnif (constructors, ty) ->
        VariantUnif (remove_variant constructors, ty)
    | [], VariantSkol (constructors, ty) ->
        VariantSkol (remove_variant constructors, ty)
    | [], VariantVar (constructors, ty) ->
        VariantVar (remove_variant constructors, ty)
    | [], _ ->
        panic __LOC__
          ("Non-variant type at the end of a pattern path: "
         ^ Typed.pretty_type ty)
    | Pattern.List :: path, List ty -> List (go path ty)
    | List :: _, ty ->
        panic __LOC__ ("Non-list at list path segment: " ^ Typed.pretty_type ty)
    | Tuple i :: path, Tuple tys -> go path tys.(i)
    | Tuple _ :: _, ty ->
        panic __LOC__
          ("Non-tuple at tuple path segment: " ^ Typed.pretty_type ty)
    | Variant (name, i) :: path, VariantClosed constructors ->
        VariantClosed (map_variant name i path constructors)
    | Variant (name, i) :: path, VariantUnif (constructors, ty) ->
        VariantUnif (map_variant name i path constructors, ty)
    | Variant (name, i) :: path, VariantSkol (constructors, ty) ->
        VariantSkol (map_variant name i path constructors, ty)
    | Variant (name, i) :: path, VariantVar (constructors, ty) ->
        VariantVar (map_variant name i path constructors, ty)
    | Variant _ :: path, ty ->
        panic __LOC__
          ("Non-variant at variant path segment: " ^ Typed.pretty_type ty)
  in
  solve_unify loc env unify_state result_type (go path ty) definition_env

let solve_interpolatable loc env state ty definition_env =
  match normalize_unif ty with
  | String
  | Number
  | Bool ->
      true
  | (Unif _ | Skol _) as ty ->
      state.deferred_constraints :=
        Difflist.snoc
          !(state.deferred_constraints)
          (Interpolatable (loc, ty, env));
      false
  | ty -> raise (TypeError (loc, NonInterpolatable ty))

let is_valid_instance env given_loc (given_constraint : class_constraint)
    evidence_source wanted_loc (wanted_constraint : class_constraint)
    evidence_target =
  let exception DoesNotMatch in
  let substitution = ref NameMap.empty in
  let rec match_types given_type wanted_type =
    let wanted_type = normalize_unif wanted_type in
    match normalize_unif given_type with
    | Forall (name, ty) -> todo __LOC__
    | Fun (arguments1, result1) -> begin
        match wanted_type with
        | Fun (arguments2, result2)
          when List.compare_lengths arguments1 arguments2 = 0 ->
            List.iter2 match_types arguments1 arguments2;
            match_types result1 result2
        | _ -> raise DoesNotMatch
      end
    (* This *doesn't* validate kinds yet *)
    | Constraint (class_constraint_type1, ty1) -> begin
        match wanted_type with
        | Constraint (class_constraint_type2, ty2) ->
            match_types class_constraint_type1 class_constraint_type2;
            match_types ty1 ty2
        | _ -> raise DoesNotMatch
      end
    | TyVar name -> begin
        match NameMap.find_opt name !substitution with
        | None -> substitution := NameMap.add name wanted_type !substitution
        | Some substituted -> match_types substituted wanted_type
      end
    | TyConstructor (name, arguments) -> begin
        match wanted_type with
        | TyConstructor (name2, arguments2) when Name.equal name name2 ->
            List.iter2 match_types arguments arguments2
        | _ -> raise DoesNotMatch
      end
    | TypeAlias { underlying; _ } -> match_types underlying wanted_type
    | Unif (typeref, _) -> begin
        match wanted_type with
        | Unif (typeref2, _) when Typeref.equal typeref typeref2 -> ()
        | _ -> raise DoesNotMatch
      end
    | Skol (unique, _, _) -> begin
        match wanted_type with
        | Skol (unique2, _, _) when Unique.equal unique unique2 -> ()
        | _ -> raise DoesNotMatch
      end
    | Number -> begin
        match wanted_type with
        | Number -> ()
        | _ -> raise DoesNotMatch
      end
    | Bool -> begin
        match wanted_type with
        | Bool -> ()
        | _ -> raise DoesNotMatch
      end
    | String -> begin
        match wanted_type with
        | String -> ()
        | _ -> raise DoesNotMatch
      end
    | Exception -> begin
        match wanted_type with
        | Exception -> ()
        | _ -> raise DoesNotMatch
      end
    | Tuple types1 -> begin
        match wanted_type with
        | Tuple types2 when Array.length types1 = Array.length types2 ->
            Array.iter2 match_types types1 types2
        | _ -> raise DoesNotMatch
      end
    | List argument1 -> begin
        match wanted_type with
        | List argument2 -> match_types argument1 argument2
        | _ -> raise DoesNotMatch
      end
    | Ref argument1 -> begin
        match wanted_type with
        | Ref argument2 -> match_types argument1 argument2
        | _ -> raise DoesNotMatch
      end
    | Promise argument1 -> begin
        match wanted_type with
        | Promise argument2 -> match_types argument1 argument2
        | _ -> raise DoesNotMatch
      end
    | RecordClosed _ -> todo __LOC__
    | RecordUnif _ -> todo __LOC__
    | RecordSkol _ -> todo __LOC__
    | RecordVar _ -> todo __LOC__
    | VariantClosed _ -> todo __LOC__
    | VariantUnif _ -> todo __LOC__
    | VariantSkol _ -> todo __LOC__
    | VariantVar _ -> todo __LOC__
    | ModSubscriptTyCon (_, _, _, _) -> .
  in

  match
    List.iter2 match_types given_constraint.arguments
      wanted_constraint.arguments
  with
  | () -> Some !substitution
  | exception DoesNotMatch -> None

let rec solve_wanted_class (wanted_loc : loc) env unify_state wanted givens
    local_env evidence_target =
  match NameMap.find_opt wanted.class_name givens.class_instances with
  | None -> todo __LOC__
  | Some trie -> (
      match InstanceTrie.match_instance trie wanted.arguments with
      | `Found instance ->
        Evidence.fill_target evidence_target instance.evidence;
        true
      | `Ambiguous instances -> raise (TypeError(wanted_loc, AmbiguousClassConstraint (wanted, todo __LOC__)))
      | `Missing ->
          (* TODO: This should be able to track if the instance might be unblocked by unification variables or skolems
             later and only requeue the wanted constraint in that case. *)
          unify_state.deferred_constraints :=
            Difflist.snoc
              !(unify_state.deferred_constraints)
              (WantedClass
                 {
                   loc = wanted_loc;
                   wanted;
                   givens;
                   local_env;
                   evidence_target;
                 });
          false)

let residuals_to_errors : ty_constraint list -> (loc * type_error) list =
 fun residuals ->
  let residual_to_error = function
    (* TODO: Actually keep track of the unification context *)
    | Unify (loc, expected, actual, _) ->
        (loc, UnableToUnify ((expected, actual), None))
    | Unwrap (loc, to_unwrap, result, _) -> (loc, CannotUnwrapNonData to_unwrap)
    | ProgramArg (loc, argument_type, _) ->
        (loc, NonProgramArgument argument_type)
    | RefineVariant _ -> panic __LOC__ "residual RefineVariant constraint"
    | Interpolatable (loc, interpolatable_type, _) ->
        (loc, NonInterpolatable interpolatable_type)
    | WantedClass { loc; wanted = class_constraint; _ } ->
        (loc, MissingInstance class_constraint)
  in

  List.map residual_to_error residuals

let solve_constraints :
    local_env -> ty_constraint list -> (loc * type_error) list =
 fun env constraints ->
  let go unify_state constraints =
    List.fold_left
      begin
        fun progress constraint_ ->
          let new_progress =
            match constraint_ with
            | Unify (loc, ty1, ty2, definition_env) ->
                solve_unify loc env unify_state ty1 ty2 definition_env
            | Unwrap (loc, ty1, ty2, definition_env) ->
                solve_unwrap loc env unify_state ty1 ty2 definition_env
            | ProgramArg (loc, ty, definition_env) ->
                solve_program_arg loc env unify_state ty definition_env
            | RefineVariant (loc, ty, path, variant, result_type, definition_env)
              ->
                solve_refine_variant loc env unify_state ty path variant
                  result_type definition_env
            | Interpolatable (loc, ty, definition_env) ->
                solve_interpolatable loc env unify_state ty definition_env
            | WantedClass { loc; wanted; givens; local_env; evidence_target } ->
                solve_wanted_class loc env unify_state wanted givens local_env
                  evidence_target
          in
          new_progress || progress
      end
      false constraints
  in

  let rec actually_solve_constraints = function
    | [] -> []
    | constraints ->
        let unify_state = { deferred_constraints = ref Difflist.empty } in
        let progress = go unify_state constraints in
        if not progress then
          residuals_to_errors
            (Difflist.to_list !(unify_state.deferred_constraints))
        else
          actually_solve_constraints
            (Difflist.to_list !(unify_state.deferred_constraints))
  in
  actually_solve_constraints constraints

let free_unifs : ty -> TyperefSet.t =
 fun ty ->
  let traversal =
    object
      inherit [TyperefSet.t] Traversal.traversal

      method! ty state ty =
        match normalize_unif ty with
        | Unif (typeref, name) as ty ->
            (ty, TyperefSet.add (typeref, name) state)
        | ty -> (ty, state)
    end
  in
  let _, typerefs = traversal#traverse_type TyperefSet.empty ty in
  typerefs

(** Generalizes a given type by turning residual unification variables into
    forall-bound type variables. 
    Generalize takes an environment since it is not allowed to generalize
    type variables at a lower level  *)
let generalize : local_env -> ty -> ty =
 fun env ty ->
  let ty' =
    TyperefSet.fold
      (fun (typeref, name) r ->
        match Typeref.get typeref with
        | Bound _ ->
            panic __LOC__
              ("Trying to generalize bound type variable: "
              ^ pretty_type (Unif (typeref, name)))
        | Unbound level
          when Typeref.generalizable_level ~ambient:env.level level ->
            let new_name = Name.refresh name in
            bind_unchecked typeref name (TyVar new_name);
            Forall (new_name, r)
        | Unbound _ -> r)
      (free_unifs ty) ty
  in
  trace_tc (lazy ("[generalize] " ^ pretty_type ty ^ " ==> " ^ pretty_type ty'));
  ty'

let close_variant ty =
  match normalize_unif ty with
  | Unif (ref, name)
  | VariantUnif (_, (ref, name)) ->
      bind_unchecked ref name (VariantClosed [||])
  | _ -> ()

let check_exhaustiveness_and_close_variants_in_exprs expr =
  let check_column loc patterns =
    begin
      match
        Pattern.check_exhaustiveness_and_close_variants ~close_variant patterns
      with
      | () -> ()
      | exception Pattern.PatternError err ->
          raise (TypeError (loc, PatternError err))
    end
  in

  let traversal =
    object
      inherit [unit] Typed.Traversal.traversal

      method! expr () expr =
        match expr with
        | Typed.Match (loc, _, patterns) ->
            check_column loc (List.map fst patterns);
            (expr, ())
        | Typed.LetSeq (loc, pattern, _) ->
            check_column (Typed.get_pattern_loc pattern) [ pattern ];
            (expr, ())
        | Typed.LetRecSeq (_, _, _, patterns, _) ->
            (* The patterns should be independent and irrefutable so we treat them each as individual columns *)
            List.iter
              (fun pattern ->
                check_column (Typed.get_pattern_loc pattern) [ pattern ])
              patterns;
            (expr, ())
        | Typed.LetInstanceSeq (_, _, _, _, _, methods) ->
            (* TODO: Account for entailment *)
            List.iter
              (fun (_, _, patterns, _) ->
                List.iter
                  (fun pattern ->
                    check_column (Typed.get_pattern_loc pattern) [ pattern ])
                  patterns)
              methods;
            (expr, ())
        | expr -> (expr, ())
    end
  in

  let _ = traversal#traverse_expr () expr in
  ()

let typecheck_top_level :
    global_env ->
    [ `Check | `Infer ] ->
    expr ->
    global_env * Typed.expr * (loc * type_error) Difflist.t =
 fun global_env check_or_infer expr ->
  let data_definitions_with_levels =
    NameMap.map
      (fun (params, underlying) ->
        (global_env.ambient_level, params, underlying))
      global_env.data_definitions
  in

  let local_env =
    {
      local_types = global_env.var_types;
      module_var_contents = global_env.module_var_contents;
      constraints = ref Difflist.empty;
      given_constraints = global_env.given_constraints;
      data_definitions = data_definitions_with_levels;
      exception_definitions = global_env.exception_definitions;
      type_aliases = global_env.type_aliases;
      type_classes = global_env.type_classes;
      level = global_env.ambient_level;
    }
  in

  let local_env_trans, typed_expr =
    check_seq_expr local_env check_or_infer expr
  in

  (* This is *extremely hacky* right now.
      We temporarily construct a fake local environment to figure out the top-level local type bindings.
      We then extract those, throw away the collected constraints (they're part of the real local_env anyway)
      and add the bindings to the global environment
  *)
  let temp_local_env =
    local_env_trans
      {
        local_types = NameMap.empty;
        module_var_contents = NameMap.empty;
        constraints = local_env.constraints;
        given_constraints = local_env.given_constraints;
        data_definitions = NameMap.empty;
        exception_definitions = NameMap.empty;
        type_aliases = NameMap.empty;
        type_classes = NameMap.empty;
        level = global_env.ambient_level;
      }
  in

  let errors =
    solve_constraints local_env (Difflist.to_list !(local_env.constraints))
  in

  check_exhaustiveness_and_close_variants_in_exprs typed_expr;

  let local_types =
    if binds_value expr then
      NameMap.map (generalize temp_local_env) temp_local_env.local_types
    else temp_local_env.local_types
  in

  let data_definitions_without_levels =
    NameMap.map
      (fun (_level, params, underlying) -> (params, underlying))
      temp_local_env.data_definitions
  in
  let global_env =
    {
      var_types =
        NameMap.union (fun _ _ x -> Some x) global_env.var_types local_types;
      module_var_contents =
        NameMap.union
          (fun _ _ x -> Some x)
          global_env.module_var_contents temp_local_env.module_var_contents;
      data_definitions =
        NameMap.union
          (fun _ _ x -> Some x)
          global_env.data_definitions data_definitions_without_levels;
      exception_definitions =
        NameMap.union
          (fun _ _ x -> Some x)
          global_env.exception_definitions temp_local_env.exception_definitions;
      type_aliases =
        NameMap.union
          (fun _ _ x -> Some x)
          global_env.type_aliases temp_local_env.type_aliases;
      type_classes =
        NameMap.union
          (fun _ _ x -> Some x)
          global_env.type_classes temp_local_env.type_classes;
      ambient_level = temp_local_env.level;
      given_constraints = temp_local_env.given_constraints;
    }
  in
  (global_env, typed_expr, Difflist.of_list errors)

let typecheck_exports : Renamed.export_item list -> Typed.export_item list =
  Obj.magic

let typecheck_header header env =
  let insert_global_var var ty env =
    { env with var_types = NameMap.add var ty env.var_types }
  in

  let add_var_content env flag_def =
    let env, args =
      match flag_def.args with
      | Varargs name ->
          (insert_global_var name (List String) env, Typed.Varargs name)
      | Switch name -> (insert_global_var name Bool env, Switch name)
      | Named names ->
          ( List.fold_left
              (fun env name -> insert_global_var name String env)
              env names,
            Named names )
      | NamedDefault names_and_values ->
          ( List.fold_left
              (fun env (name, _) -> insert_global_var name String env)
              env names_and_values,
            NamedDefault names_and_values )
    in
    ( env,
      Typed.{ args; description = flag_def.description; flags = flag_def.flags }
    )
  in
  let env, options = List.fold_left_map add_var_content env header.options in
  ( env,
    Typed.
      {
        options;
        usage = header.usage;
        description = header.description;
        exports = typecheck_exports header.exports;
      } )

let typecheck check_or_infer_top_level header exprs global_env =
  trace_tc
    (lazy
      ((match check_or_infer_top_level with
       | `Infer -> "Inferring"
       | `Check -> "Checking")
      ^ " top level definitions"));

  let global_env, header = typecheck_header header global_env in

  let (global_env, errors), exprs =
    List.fold_left_map
      (fun (env, errors) expr ->
        let env, expr, new_errors =
          typecheck_top_level env check_or_infer_top_level expr
        in
        ((env, Difflist.append errors new_errors), expr))
      (global_env, Difflist.empty)
      exprs
  in
  (global_env, header, exprs, Difflist.to_list errors)

let prim_types =
  NameMap.of_seq
    (Seq.map
       (fun (name, ty) -> ({ name; index = Name.primop_index }, ty))
       (Primops.PrimOpMap.to_seq Primops.primops))

(* TODO: Maybe primops should be part of an implicitly imported module? *)
let empty_env =
  {
    var_types = prim_types;
    module_var_contents = NameMap.empty;
    data_definitions = NameMap.empty;
    exception_definitions =
      NameMap.of_seq
        (Seq.map
           (fun (_, (name, param_types)) -> (name, param_types))
           (StringMap.to_seq Primops.prim_exceptions));
    type_aliases = NameMap.empty;
    type_classes = NameMap.empty;
    ambient_level = Typeref.initial_top_level;
    given_constraints = { class_instances = NameMap.empty };
  }
