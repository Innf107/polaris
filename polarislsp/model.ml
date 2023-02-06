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


type hover_entry = Var of name * ty
                 | VarPattern of name * ty

type model = {
  (* Since matching in the map is a little fuzzy, we need to store the exact
     location as a value *)
  hover_entries_at_loc : (loc * hover_entry) LocOverlapMap.t
}

type t = model

let build exprs =
  let module Difflist = Polaris.Difflist in
  let var_type_traversal = object 
    inherit [(loc * (loc * hover_entry)) Difflist.t] Traversal.traversal

    method! expr var_types expr =
      match expr with
      | Typed.Var ((loc, ty), name) -> 
        expr, Difflist.append var_types (Difflist.of_list [(loc, (loc, Var (name, ty)))])
      | _ -> expr, var_types

    method! pattern var_types pattern =
      match pattern with
      | Typed.VarPat ((loc, ty), name) ->
        pattern, Difflist.append var_types (Difflist.of_list [(loc, (loc, VarPattern (name, ty)))])
      | _ -> pattern, var_types
  end
  in
  let _, var_types = var_type_traversal#traverse_list var_type_traversal#traverse_expr Difflist.empty exprs in
  { hover_entries_at_loc = LocOverlapMap.of_seq (Difflist.to_seq var_types) }

let find_hover_entry_at ~file (line, column) model =
  (* LSP locations are zero-based! *)
  let loc = Polaris.Loc.{ 
      file;
      start_line = line + 1; 
      end_line = line + 1;
      start_col = column + 1;
      end_col = column + 1;
    }
  in
  LocOverlapMap.find_opt loc model.hover_entries_at_loc
