open Polaris.Syntax
open Polaris.Syntax.Typed

module LocByOverlap = struct
  type t = loc

  let compare loc1 loc2 =
    let open Polaris.Loc in
    match String.compare loc1.file loc2.file with
    | 0
      when loc1.end_line < loc2.start_line
           || (loc1.end_line = loc2.start_line && loc1.end_col < loc2.start_col)
      ->
        -1
    | 0
      when loc2.end_line < loc1.start_line
           || (loc2.end_line = loc1.start_line && loc2.end_col < loc1.start_col)
      ->
        1
    | x -> x
end

module LocOverlapMap = Map.Make (LocByOverlap)

module PositionMap = Map.Make (struct
  type t = Polaris.Loc.position

  let compare = Polaris.Loc.compare_position
end)

module ScopeCollection = struct
  type t = Polaris.Rename.RenameScope.t PositionMap.t
  type collector = Polaris.Rename.RenameScope.t PositionMap.t ref

  let new_collector () = ref PositionMap.empty

  let registration_for collector =
    Polaris.Rename.
      {
        register_scope =
          (fun position scope ->
            collector := PositionMap.add position scope !collector);
        register_reset_scope =
          (fun position scope ->
            collector :=
              PositionMap.update position
                (function
                  | None -> Some scope
                  | Some previous -> Some previous)
                !collector);
      }

  let collect collector = !collector

  let find_at_or_before :
      Polaris.Loc.position -> t -> Polaris.Rename.RenameScope.t =
   fun pos scopes ->
    match
      PositionMap.find_last_opt
        (fun loc_candidate ->
          Polaris.Loc.compare_position loc_candidate pos <= 0)
        scopes
    with
    | Some (_, scope) -> scope
    | None ->
        Polaris.Util.panic __LOC__
          ("No scope found for "
          ^ Polaris.Loc.(
              string_of_int pos.line ^ " | " ^ string_of_int pos.column))
end

type hover_entry =
  | VarLike of name * ty
  | Subscript of string * ty
  | Variant of string * ty

type model = {
  (* Since matching in the map is a little fuzzy, we need to store the exact
     location as a value *)
  hover_entries_at_loc : (loc * hover_entry) LocOverlapMap.t;
  definitions_at_loc : (loc * loc) LocOverlapMap.t;
  scopes : ScopeCollection.t;
}

type t = model

let hover_entry_traversal =
  let module Difflist = Polaris.Difflist in
  object
    inherit [(loc * (loc * hover_entry)) Difflist.t] Traversal.traversal

    method! expr hover_entries expr =
      match expr with
      | Typed.Var (((loc, ty), _), name)
      | Typed.DataConstructor ((loc, ty), name)
      | Typed.ModSubscript ((loc, ty), _, name) ->
          (expr, Difflist.snoc hover_entries (loc, (loc, VarLike (name, ty))))
          (* Subscripts use the location of the field, not the entire expression *)
      | Typed.Subscript ((loc, ty), _, key) ->
          ( expr,
            Difflist.snoc hover_entries
              (loc.subloc, (loc.subloc, Subscript (key, ty))) )
      | Typed.LetRecSeq ((loc, ty), _, name, _, _) ->
          ( expr,
            Difflist.snoc hover_entries
              (loc.subloc, (loc.subloc, VarLike (name, ty))) )
      | _ -> (expr, hover_entries)

    method! pattern hover_entries pattern =
      match pattern with
      | Typed.VarPat ((loc, ty), name) ->
          ( pattern,
            Difflist.append hover_entries
              (Difflist.of_list [ (loc, (loc, VarLike (name, ty))) ]) )
      | Typed.VariantPat (({ subloc; _ }, ty), name, _) ->
          ( pattern,
            Difflist.append hover_entries
              (Difflist.of_list [ (subloc, (subloc, Variant (name, ty))) ]) )
      | _ -> (pattern, hover_entries)
  end

let definition_entry_traversal =
  let module Difflist = Polaris.Difflist in
  object
    inherit [(loc * (loc * loc)) Difflist.t] Traversal.traversal

    method! expr entries expr =
      match expr with
      | Var (((loc, _), definition_loc), _) ->
          (expr, Difflist.snoc entries (loc, (loc, definition_loc)))
      | _ -> (expr, entries)
  end

let build scopes exprs =
  let module Difflist = Polaris.Difflist in
  let _, hover_entries =
    Traversal.traverse_list hover_entry_traversal#traverse_expr Difflist.empty
      exprs
  in
  let _, definition_entries =
    Traversal.traverse_list definition_entry_traversal#traverse_expr
      Difflist.empty exprs
  in
  {
    hover_entries_at_loc = LocOverlapMap.of_seq (Difflist.to_seq hover_entries);
    definitions_at_loc =
      LocOverlapMap.of_seq (Difflist.to_seq definition_entries);
    scopes;
  }

let find_hover_entry_at ~file Lsp.{ line; character } model =
  (* LSP locations are zero-based! *)
  let loc =
    Polaris.Loc.
      {
        file;
        start_line = line + 1;
        end_line = line + 1;
        start_col = character + 1;
        end_col = character + 1;
      }
  in
  LocOverlapMap.find_opt loc model.hover_entries_at_loc

let find_definition_at ~file Lsp.{ line; character } model =
  let loc =
    Polaris.Loc.
      {
        file;
        start_line = line + 1;
        end_line = line + 1;
        start_col = character + 1;
        end_col = character + 1;
      }
  in
  LocOverlapMap.find_opt loc model.definitions_at_loc

let loc_position_of_range_position (position : Lsp.position) =
  Polaris.Loc.{ line = position.line + 1; column = position.character + 1 }

let find_scope_at_or_before :
    Lsp.position -> model -> Polaris.Rename.RenameScope.t =
 fun pos model ->
  ScopeCollection.find_at_or_before
    (loc_position_of_range_position pos)
    model.scopes
