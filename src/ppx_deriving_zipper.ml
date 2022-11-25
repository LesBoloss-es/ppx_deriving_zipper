let handle_type_decl type_decl =
  Format.eprintf "%a\n;;@." Pprintast.structure_item
    (Ast_helper.Str.type_ Asttypes.Recursive [type_decl]);
  let syntax = Syntax.Parse.type_declaration type_decl in
  let decl = Types.Parse.decl syntax in
  Format.eprintf "%a\n;;@." Types.pp_decl decl;
  (* List of structure items to generate *)
  let stris =
    (* type declarations *)
    (Type_gen.type_gen decl |> List.map Syntax.Print.type_declaration)
    @ (* code *)
    [Code_gen.zip decl]
  in
  List.iter (Pprintast.structure_item Format.err_formatter) stris;
  exit 0

let type_decl_str ~options:_ ~path:_ =
  function
    (* FIXME: handle several type declarations (mutually rec types?) *)
    | [type_decl] -> handle_type_decl type_decl
    | _ -> assert false

let () = Ppx_deriving.(register (create ~type_decl_str "zipper" ()))
