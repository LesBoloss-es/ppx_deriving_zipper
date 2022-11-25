let debug args = Format.kasprintf (Format.eprintf "[DEBUG] %s@.") args

let handle_type_decl type_decl : Ppxlib.structure =
  debug "%a" Pprintast.structure_item
    (Ast_helper.Str.type_ Asttypes.Recursive [type_decl]);
  let decl = Syntax.Parse.type_declaration type_decl |> Types.Parse.decl in
  debug "%a" Types.pp_decl decl;
  (* List of structure items to generate *)
  (* type declarations *)
  (Type_gen.type_gen decl |> List.map Syntax.Print.type_declaration)
  @ (* code *)
  [Code_gen.zip decl]

let type_decl_str ~options:_ ~path:_ = function
  (* FIXME: handle several type declarations (mutually rec types?) *)
  | [type_decl] -> handle_type_decl type_decl
  | _ -> assert false

let () = Ppx_deriving.(register (create ~type_decl_str "zipper" ()))
