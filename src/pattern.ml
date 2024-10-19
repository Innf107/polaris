open Syntax
open Util

let _category, trace = Trace.make ~flag:"patterns" ~prefix:"Patterns"

module StringSet = Set.Make (String)

type pattern_error =
  | ListWithoutNil
  | ListWithoutCons
  | ExceptionWithoutWildcard
  | NumWithoutWildcard
  | StringWithoutWildcard
  | BoolWithout of bool
  | VariantNonExhaustive of string list

exception PatternError of pattern_error

(* Matrices are represented by rows of non-empty lists of columns
   INVARIANT: all rows in a pattern matrix always have the same number of columns *)
type matrix = (Typed.pattern * Typed.pattern list) list

let as_pattern_matrix patterns =
  let rec flatten_pattern =
    let ( let* ) list cont = List.concat_map cont list in
    function
    (* We can remove as and type patterns since these are not relevant for closing variants.
       The same holds for data patterns since there is only ever a single
       non-wildcard-like data pattern for a given type *)
    | Typed.AsPat (_, underlying, _)
    | Typed.TypePat (_, underlying, _)
    | Typed.DataPat (_, _, underlying) ->
        flatten_pattern underlying
    (* or patterns are exploded out.
       TODO: This might be a bit inefficient (as in, like, exponential... let's hope that's not going to be an issue) *)
    | Typed.OrPat (_, left, right) ->
        flatten_pattern left @ flatten_pattern right
    | Typed.VarPat (ty, name) -> [ Typed.VarPat (ty, name) ]
    | Typed.ConsPat (loc, head, tail) ->
        let* flat_head = flatten_pattern head in
        let* flat_tail = flatten_pattern tail in
        [ Typed.ConsPat (loc, flat_head, flat_tail) ]
    | Typed.ListPat (loc, elements) ->
        let* elements = Classes.MonadList.traverse flatten_pattern elements in
        (* Non-empty list patterns are desugared to :: patterns *)
        [
          List.fold_right
            (fun pattern rest -> Typed.ConsPat (loc, pattern, rest))
            elements
            (ListPat (loc, []));
        ]
    | Typed.TuplePat (loc, elements) ->
        let* elements = Classes.MonadList.traverse flatten_pattern elements in
        [ Typed.TuplePat (loc, elements) ]
    | (Typed.NumPat _ | Typed.StringPat _ | Typed.BoolPat _) as pattern ->
        [ pattern ]
    | Typed.ExceptionDataPat (loc, name, patterns) ->
        let* patterns = Classes.MonadList.traverse flatten_pattern patterns in
        [ Typed.ExceptionDataPat (loc, name, patterns) ]
    | Typed.VariantPat (loc, name, patterns) ->
        let* patterns = Classes.MonadList.traverse flatten_pattern patterns in
        [ Typed.VariantPat (loc, name, patterns) ]
  in

  List.map (fun x -> (x, [])) (List.concat_map flatten_pattern patterns)

let as_non_empty = function
  | [] -> None
  | x :: xs -> Some (x, xs)

let wildcard_like = function
  | Typed.VarPat _ -> true
  | AsPat _ -> panic __LOC__ "as pattern found after simplification"
  | OrPat _ -> panic __LOC__ "or pattern found after simplification"
  | TypePat _ -> panic __LOC__ "type pattern found after simplification"
  | DataPat _ -> panic __LOC__ "data pattern found after simplification"
  | ConsPat _
  | ListPat _
  | TuplePat _
  | NumPat _
  | StringPat _
  | BoolPat _
  | ExceptionDataPat _
  | VariantPat _ ->
      false

let sub_patterns ?(count = 0) = function
  | Typed.VarPat _ as pattern -> Util.replicate count pattern
  | ConsPat (_, head, tail) -> [ head; tail ]
  | ListPat (_, elements) -> elements
  | TuplePat (_, elements) -> elements
  | NumPat _ -> []
  | StringPat _ -> []
  | BoolPat _ -> []
  | ExceptionDataPat (_, _, patterns) -> patterns
  | VariantPat (_, _, patterns) -> patterns
  | AsPat _ -> panic __LOC__ "as pattern found after simplification"
  | OrPat _ -> panic __LOC__ "or pattern found after simplification"
  | TypePat _ -> panic __LOC__ "type pattern found after simplification"
  | DataPat _ -> panic __LOC__ "data pattern found after simplification"

(* ~exact specifies if this should only return true if two patterns contain exactly the same head
   constructor or if wildcard-likes should be compatible with everything *)
let compatible_head_pattern :
    exact:bool -> Typed.pattern -> Typed.pattern -> bool =
 fun ~exact reference pattern ->
  match (reference, pattern) with
  | VarPat _, VarPat _ -> true
  | VarPat _, _
  | _, VarPat _ ->
      not exact
  | ConsPat _, ConsPat _ -> true
  (* Non-empty list patterns have been desugared already *)
  | ListPat (_, []), ListPat (_, []) -> true
  | TuplePat _, TuplePat _ -> true
  | NumPat (_, x), NumPat (_, y) -> x = y
  | StringPat (_, x), StringPat (_, y) -> String.equal x y
  | ExceptionDataPat (_, name, _), ExceptionDataPat (_, name2, _) ->
      Name.equal name name2
  | VariantPat (_, name, _), VariantPat (_, name2, _) -> String.equal name name2
  | _ -> false

let reduce_head_pattern head_pattern (matrix : matrix) =
  let head_sub_pattern_count = List.length (sub_patterns head_pattern) in
  let reduce (head, rest) =
    if compatible_head_pattern ~exact:false head head_pattern then
      as_non_empty (sub_patterns ~count:head_sub_pattern_count head @ rest)
    else None
  in
  List.filter_map reduce matrix

(* WARNING: This is quadratic right now!
   TODO: Use a Set or something to improve the complexity *)
let rec deduplicate_head_patterns = function
  | [] -> []
  | pattern :: rest ->
      pattern
      :: deduplicate_head_patterns
           (List.filter
              (fun other ->
                not (compatible_head_pattern ~exact:true pattern other))
              rest)

let check_and_close_column ~close_variant = function
  | [] ->
      panic __LOC__ "check_and_close_column: called on an empty pattern column"
      (* Any pattern row containing wildcards is automatically exhaustive
         and can stay open if it also matches variants *)
  | patterns when List.exists wildcard_like patterns -> ()
  | pattern :: remaining -> (
      match pattern with
      (* Tuple patterns are irrefutable and therefore always exhaustive *)
      | TuplePat _ -> ()
      (* Matching on exceptions, numbers and strings without a wildcard case is necessarily non-exhaustive *)
      | ExceptionDataPat _ -> raise (PatternError ExceptionWithoutWildcard)
      | NumPat _ -> raise (PatternError NumWithoutWildcard)
      | StringPat _ -> raise (PatternError StringWithoutWildcard)
      | BoolPat (_, literal) ->
          if
            List.exists
              (function
                | Typed.BoolPat (_, other) when other = not literal -> true
                | _ -> false)
              remaining
          then ()
          else raise (PatternError (BoolWithout (not literal)))
      | ConsPat _ ->
          if
            List.exists
              (function
                | Typed.ListPat (_, []) -> true
                | _ -> false)
              remaining
          then ()
          else raise (PatternError ListWithoutNil)
      | ListPat _ ->
          if
            List.exists
              (function
                | Typed.ConsPat _ -> true
                | _ -> false)
              remaining
          then ()
          else raise (PatternError ListWithoutCons)
      | VariantPat ((loc, ty), name, _) ->
          trace (lazy ("closing variant type: " ^ Typed.pretty_type ty));
          close_variant ty;
          let variant_constructors =
            match Typed.Ty.normalize_unif ty with
            | Typed.VariantClosed constructors
            | Typed.VariantUnif (constructors, _)
            | Typed.VariantVar (constructors, _)
            | Typed.VariantSkol (constructors, _) ->
                StringSet.of_seq (Array.to_seq (Array.map fst constructors))
            | ty ->
                panic __LOC__
                  (Loc.pretty loc.main
                 ^ ": variant pattern with non-variant type: "
                 ^ Typed.pretty_type ty)
          in
          let rec missing_constructors variant_constructors = function
            | [] -> variant_constructors
            | Typed.VariantPat ((loc, ty), name, _) :: remaining ->
                missing_constructors
                  (StringSet.remove name variant_constructors)
                  remaining
            | pattern :: _ ->
                panic __LOC__
                  (Loc.pretty loc.main
                 ^ ": non-variant and non-wildcard pattern in variant pattern \
                    match")
          in

          begin
            match
              missing_constructors
                (StringSet.remove name variant_constructors)
                remaining
            with
            | missing when StringSet.is_empty missing -> ()
            | missing ->
                raise
                  (PatternError
                     (VariantNonExhaustive
                        (List.of_seq (StringSet.to_seq missing))))
          end
      | AsPat _ -> panic __LOC__ "as pattern after simplification"
      | TypePat _ -> panic __LOC__ "type pattern after simplification"
      | DataPat _ -> panic __LOC__ "data pattern after simplification"
      | OrPat _ -> panic __LOC__ "or pattern after simplification"
      | VarPat _ ->
          panic __LOC__
            "variable pattern in ostensibly non-wildcard pattern column")

let check_exhaustiveness_and_close_variants ~close_variant patterns =
  trace
    (lazy
      ("unprocessed patterns: "
      ^ String.concat " | " (List.map Typed.pretty_pattern patterns)));
  let matrix = as_pattern_matrix patterns in
  trace
    (lazy
      ("simplified matrix: "
      ^ String.concat " | "
          (List.map
             (fun (x, rest) ->
               "(" ^ Typed.pretty_pattern x ^ ", {"
               ^ String.concat " | " (List.map Typed.pretty_pattern rest)
               ^ "})")
             matrix)));

  let rec process_matrix = function
    | [] -> ()
    | matrix ->
        trace
          (lazy
            ("matrix: "
            ^ String.concat " | "
                (List.map
                   (fun (x, rest) ->
                     "(" ^ Typed.pretty_pattern x ^ ", {"
                     ^ String.concat " | " (List.map Typed.pretty_pattern rest)
                     ^ "})")
                   matrix)));
        let head_patterns =
          deduplicate_head_patterns
            (List.map (fun (pattern, _) -> pattern) matrix)
        in
        trace
          (lazy
            ("head_patterns: "
            ^ String.concat " | " (List.map Typed.pretty_pattern head_patterns)
            ));

        check_and_close_column ~close_variant head_patterns;

        let continue pattern =
          if wildcard_like pattern then (* TODO: Is this correct? *)
            ()
          else process_matrix (reduce_head_pattern pattern matrix)
        in

        List.iter continue head_patterns
  in

  process_matrix matrix

type path_segment =
  | List
  | Tuple of int
  | Variant of string * int

type path = path_segment list

let pretty_path path =
  let pretty_segment = function
    | List -> "List"
    | Tuple int -> "Tuple(" ^ string_of_int int ^ ")"
    | Variant (constructor, int) ->
        "Variant(" ^ constructor ^ ", " ^ string_of_int int ^ ")"
  in
  String.concat " -> " (List.map pretty_segment path)

let pretty_refutability = function
  | `Irrefutable -> "irrefutable"
  | `Refutable -> "refutable"
  | `Variant (constructor, path) ->
      "variant(" ^ constructor ^ "): " ^ pretty_path path

let check_variant_refutability : Typed.pattern -> (path * string) option =
  let rec try_all f = function
    | [] -> `Irrefutable
    | x :: xs -> (
        match f x with
        | `Irrefutable -> try_all f xs
        | `Refutable -> `Refutable
        | `Variant _ as variant -> (
            match try_all f xs with
            | `Refutable -> `Refutable
            | `Irrefutable -> variant
            (* If more than one variant pattern is refutable, we need to treat the entire
               pattern as refutable *)
            | `Variant _ -> `Refutable))
  in

  (* TODO: Closed variants with a single constructor are also irrefutable, but we don't necessarily *know* which ones should
     be closed yet. This *should* at least work for patterns with explicit type signatures for now though *)
  let rec go path = function
    (* The interesting bit *)
    | Typed.VariantPat ((_, variant_ty), name, patterns) ->
        let inner_refutability =
          try_all
            (fun (i, pattern) -> go (Variant (name, i) :: path) pattern)
            (List.mapi (fun i pattern -> (i, pattern)) patterns)
        in
        begin
          match (inner_refutability, variant_ty) with
          (* If the type of the outer pattern is closed  and only contains single variant constructor,
             we can treat it as irrefutable and focus on the inner pattern instead.
             Note that this may not always fire, since we don't necessarily know if the variant is closed at this point *)
          | `Variant _, VariantClosed [| _ |] -> inner_refutability
          | `Irrefutable, _ -> `Variant (name, path)
          | (`Refutable | `Variant _), _ -> `Refutable
        end
    | VarPat _ -> `Irrefutable
    | ConsPat _
    | ListPat _
    | NumPat _
    | StringPat _
    | BoolPat _
    | ExceptionDataPat _ ->
        `Refutable
    | OrPat (_, left, right) -> begin
        match (go path left, go path right) with
        (* If the first branch is irrefutable, the entire or-pattern is irrefutable.
           TODO: This should probably emit a warning since the second branch will never match *)
        | `Irrefutable, _ -> `Irrefutable
        (* This is the only case that is technically fine. It's pretty weird though. *)
        | (`Variant _ as variant), `Irrefutable -> variant
        (* TODO: Technically, (`Variant _, `Variant _) could be fine as well if the paths and names are equivalent right? *)
        | `Refutable, _
        | `Variant _, _ ->
            `Refutable
      end
    | AsPat (_, inner, _) -> go path inner
    | TuplePat (_, patterns) ->
        try_all
          (fun (index, pattern) -> go (Tuple index :: path) pattern)
          (List.mapi (fun index pattern -> (index, pattern)) patterns)
    | TypePat (_, inner, _) -> go path inner
    | DataPat (_, _, inner) -> begin
        (* We cannot refine inside a data pattern, so we need to treat returned `Variant refutabilities
           as irrefutable.
           TODO: It might be possible to refine data patterns in the next pattern *)
        match go path inner with
        (* TODO: This should be fine if the inner Variant pattern is itself irrefutable *)
        | `Variant _ -> `Refutable
        | x -> x
      end
  in

  fun pattern ->
    let refutability = go [] pattern in
    trace (lazy ("pattern match state: " ^ pretty_refutability refutability));
    match refutability with
    | `Refutable -> None
    | `Irrefutable -> None
    | `Variant (name, path) -> Some (path, name)

type refinement =
  | Uncovered
  | RefineVariant of { cases : refinement array StringMap.t }
  | RefineTuple of refinement array
  (* We don't need to store the constructor for newtype constructors
     since it is always going to be unique for well-typed programs *)
  | RefineData of refinement
  | BoolCovered of bool
  | FullyCovered

let uncovered : refinement = Uncovered

let rec extend_refinement : refinement -> Renamed.pattern -> refinement =
 fun refinement pattern ->
  match refinement with
  | FullyCovered -> FullyCovered
  | _ -> begin
      match pattern with
      | VarPat _ -> FullyCovered
      | AsPat (_, pattern, _name) -> extend_refinement refinement pattern
      | ConsPat (loc, head_pattern, tail_pattern) -> todo __LOC__
      | ListPat _ -> todo __LOC__
      | TuplePat (loc, sub_patterns) -> begin
          match refinement with
          | Uncovered ->
              RefineTuple
                (Array.map
                   (fun sub_pattern -> extend_refinement Uncovered sub_pattern)
                   (Array.of_list sub_patterns))
          (* Technically unreachable since we already matched on FullyCovered above.
             Our coverage checker would have caught this but OCaml isn't as smart *)
          | FullyCovered -> FullyCovered
          (* should be impossible since that's a type error but we might still need to fail gracefully*)
          | RefineVariant _
          | BoolCovered _
          | RefineData _ ->
              todo __LOC__
          | RefineTuple sub_refinements ->
              RefineTuple
                (Array.map2 extend_refinement sub_refinements
                   (Array.of_list sub_patterns))
        end
      | NumPat _
      | StringPat _ ->
          refinement
      | BoolPat (_, bool) -> begin
          match refinement with
          | FullyCovered -> FullyCovered
          | Uncovered -> BoolCovered bool
          | BoolCovered previous ->
              if Bool.equal previous bool then refinement else FullyCovered
          | _ -> todo __LOC__
        end
      | OrPat (_, left, right) ->
          let left_refinement = extend_refinement refinement left in
          extend_refinement left_refinement right
          (* TODO: Can we really just ignore the type here? Is there no information
             we can extract from it wrt variants or something?*)
      | TypePat (_, pattern, _type) -> extend_refinement refinement pattern
      | DataPat _ -> todo __LOC__
      | ExceptionDataPat _ -> todo __LOC__
      (* The interesting case *)
      | VariantPat (_, variant_name, sub_patterns) -> begin
          match refinement with
          | FullyCovered -> FullyCovered
          | Uncovered ->
              RefineVariant
                {
                  cases =
                    StringMap.singleton variant_name
                      (Array.map
                         (fun sub_pattern ->
                           extend_refinement Uncovered sub_pattern)
                         (Array.of_list sub_patterns));
                }
          | RefineVariant { cases } ->
              RefineVariant
                {
                  cases =
                    cases
                    |> StringMap.update variant_name (function
                         | None ->
                             Some
                               (Array.map
                                  (fun sub_pattern ->
                                    extend_refinement Uncovered sub_pattern)
                                  (Array.of_list sub_patterns))
                         | Some refinements ->
                             Some
                               (Array.map2 extend_refinement refinements
                                  (Array.of_list sub_patterns)));
                }
          (* these should already be type errors *)
          | RefineData _
          | RefineTuple _
          | BoolCovered _ ->
              todo __LOC__
        end
    end

let collapse_coverage :
    [ `FullyCovered | `NotYetCovered ] array ->
    [ `FullyCovered | `NotYetCovered ] =
 fun array ->
  if
    Array.for_all
      (function
        | `FullyCovered -> true
        | _ -> false)
      array
  then `FullyCovered
  else `NotYetCovered

(* TODO: figure out how to handle unification variables more gracefully.
   This will probably involve making the constraint solver do another trip
   TODO: i hate type aliases... *)
let rec refine :
    normalize_unif:(Renamed.ty -> Renamed.ty) ->
    unify:(Renamed.ty -> Renamed.ty -> unit) ->
    fresh_unif:(unit -> Renamed.ty) ->
    refine_variant:(Renamed.ty -> Renamed.ty -> string list -> unit) ->
    refinement ->
    Renamed.ty ->
    Renamed.ty * [ `FullyCovered | `NotYetCovered ] =
 fun ~normalize_unif ~unify ~fresh_unif ~refine_variant refinement type_ ->
  match refinement with
  (* We cannot use boolean coverage information to meaningfully refine types *)
  | Uncovered
  | BoolCovered _ ->
      (type_, `NotYetCovered)
  | FullyCovered -> begin
      match normalize_unif type_ with
      | VariantClosed _ -> (VariantClosed [||], `FullyCovered)
      | VariantUnif (_, extension) ->
          (VariantUnif ([||], extension), `FullyCovered)
      | VariantSkol (_, extension) ->
          (VariantSkol ([||], extension), `FullyCovered)
      | VariantVar (_, extension) ->
          (VariantVar ([||], extension), `FullyCovered)
      | Unif _ ->
          todo __LOC__
          (* TODO: We technically don't need to care about the type since
              it is already fully covered anyway,
              but if we do nothing, variants behind unification variables
              will not be refined to empty variants.
              We are probably going to need constraints for this like we had
              for the old system*)
      | ty_ -> (ty_, `FullyCovered)
    end
  | RefineTuple sub_refinements -> begin
      match normalize_unif type_ with
      | Renamed.Tuple types ->
          let refined_types, coverage =
            Array.split
              (Array.map2
                 (refine ~normalize_unif ~unify ~fresh_unif ~refine_variant)
                 sub_refinements types)
          in
          (Renamed.Tuple refined_types, collapse_coverage coverage)
      | Unif _ as type_ ->
          (* We know this needs to be a tuple anyway so if the type checker hasn't
             realized this yet we might as well do it ourselves *)
          let sub_types = Array.map (fun _ -> fresh_unif ()) sub_refinements in
          unify type_ (Tuple sub_types);

          let refined_types, coverage =
            Array.split
              (Array.map2
                 (refine ~normalize_unif ~unify ~fresh_unif ~refine_variant)
                 sub_refinements sub_types)
          in
          (Renamed.Tuple refined_types, collapse_coverage coverage)
      | TypeAlias _ -> todo __LOC__
      | _ ->
          panic __LOC__
            "Pattern.refine: trying to refine non-tuple type with a tuple \
             refinement"
    end
  | RefineData sub_refinement -> todo __LOC__
  | RefineVariant { cases } -> begin
      match normalize_unif type_ with
      | VariantClosed variants ->
          let refine_variant_case (constructor, sub_types) =
            match StringMap.find_opt constructor cases with
            | None -> Some (constructor, sub_types)
            | Some sub_refinements ->
                let refined_sub_types, coverage =
                  Array.split
                    (Array.map2
                       (refine ~normalize_unif ~unify ~fresh_unif
                          ~refine_variant)
                       sub_refinements (Array.of_list sub_types))
                in
                begin
                  match collapse_coverage coverage with
                  | `FullyCovered -> None
                  | `NotYetCovered ->
                      Some (constructor, Array.to_list refined_sub_types)
                end
          in
          let remaining_cases =
            Util.filter_map_array refine_variant_case variants
          in
          begin
            match remaining_cases with
            | [||] -> (VariantClosed [||], `FullyCovered)
            | _ -> (VariantClosed remaining_cases, `NotYetCovered)
          end
      | _ -> panic __LOC__ (Typed.pretty_type type_)
    end
