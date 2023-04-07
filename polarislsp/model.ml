open Polaris.Syntax
open Polaris.Syntax.Typed

module LocByOverlap = struct 
  type t = loc
  let compare loc1 loc2 =
    let open Polaris.Loc in
    match String.compare loc1.file loc2.file with
    | 0 when loc1.end_line < loc2.start_line
      || (loc1.end_line = loc2.start_line && loc1.end_col < loc2.start_col)
      -> -1
    | 0 when loc2.end_line < loc1.start_line 
    || (loc2.end_line = loc1.start_line && loc2.end_col < loc1.start_col)
      -> 1
    | x -> x

end

module LocOverlapMap = Map.Make(LocByOverlap)


type hover_entry = VarLike of name * ty
                 | Subscript of string * ty
                 | Variant of string * ty

type model = {
  (* Since matching in the map is a little fuzzy, we need to store the exact
     location as a value *)
  hover_entries_at_loc : (loc * hover_entry) LocOverlapMap.t
}

type t = model

let build exprs =
  let module Difflist = Polaris.Difflist in
  let hover_entry_traversal = object 
    inherit [(loc * (loc * hover_entry)) Difflist.t] Traversal.traversal

    method! expr hover_entries expr =
      match expr with
      | Typed.Var ((loc, ty), name) 
      | Typed.DataConstructor ((loc, ty), name)
      | Typed.ModSubscript ((loc, ty), _, name) ->
        expr, Difflist.snoc hover_entries (loc, (loc, VarLike (name, ty)))
        (* Subscripts use the location of the field, not the entire expression *)
      | Typed.Subscript ((loc, ty), _, key) -> 
        expr, Difflist.snoc hover_entries (loc.subloc, (loc.subloc, Subscript (key, ty)))
      | Typed.LetRecSeq ((loc, ty), _, name, _, _) | Typed.LetRec ((loc, ty), _, name, _, _, _) ->
        expr, Difflist.snoc hover_entries (loc.subloc, (loc.subloc, VarLike (name, ty)))
      | _ -> expr, hover_entries

    method! pattern hover_entries pattern =
      match pattern with
      | Typed.VarPat ((loc, ty), name) ->
        pattern, Difflist.append hover_entries (Difflist.of_list [(loc, (loc, VarLike (name, ty)))])
      | Typed.VariantPat ((loc, ty), name, _) ->
        pattern, Difflist.append hover_entries (Difflist.of_list [(loc, (loc, Variant (name, ty)))])
      | _ -> pattern, hover_entries
  end
  in
  let _, hover_entries = Traversal.traverse_list hover_entry_traversal#traverse_expr Difflist.empty exprs in
  { hover_entries_at_loc = LocOverlapMap.of_seq (Difflist.to_seq hover_entries) }

let find_hover_entry_at ~file Lsp.{ line; character } model =
  (* LSP locations are zero-based! *)
  let loc = Polaris.Loc.{ 
      file;
      start_line = line + 1; 
      end_line = line + 1;
      start_col = character + 1;
      end_col = character + 1;
    }
  in
  LocOverlapMap.find_opt loc model.hover_entries_at_loc
