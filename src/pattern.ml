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

let reduce_head_pattern head_pattern matrix =
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
