open Ast_helper
open Parsetree

let wrap d = Str.type_ Asttypes.Recursive [Ztype.to_decl d]

let type_decl_str ~options ~path =
  ignore options;
  ignore path;
  function
  | [type_decl] ->
    (* sanity checks / debugging *)
    assert (List.length type_decl.ptype_cstrs = 0);
    (match type_decl.ptype_manifest with
     | None -> ()
     | Some core_type -> print_endline (Ppx_deriving.string_of_core_type core_type));

    let decl = Ztype.decl_of_type_declaration type_decl in
    let ancestor, zipper = Type_gen.all decl in
    wrap ancestor
    :: wrap zipper
    :: Code_gen.zip decl
    :: Code_gen.go_up decl ancestor
  | _ -> assert false

let () = Ppx_deriving.(register (create ~type_decl_str "zipper" ()))
