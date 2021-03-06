open Ast_helper
open Parsetree

let wrap_decl ?(non_rec=true) ~is_derivative d =
  Str.type_
    Asttypes.(if non_rec then Nonrecursive else Recursive)
    [Ztype.to_decl ~is_derivative d]

let mangle_type_decl_to_module ?(fixpoint="t") affix type_ =
  let cap = String.capitalize_ascii in
  (match affix with
   | `Prefix prefix | `PrefixSuffix (prefix, _) -> cap prefix
   | _ -> "")
  ^
  (let type_ = type_.ptype_name.txt in
   if type_ = fixpoint then "" else cap type_)
  ^
  (match affix with
   | `Suffix suffix | `PrefixSuffix (_, suffix) -> cap suffix
   | _ -> "")

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
    let derivative, ancestor, zipper = Type_gen.all decl in
    let view = Type_gen.view decl in
    let thunk =
      let loc = Location.none in
      [%stri type 'a thunk = unit -> 'a]
    in
    let in_module name str = Str.module_ (Mb.mk name (Mod.structure str)) in
    [
      in_module
        (Location.mknoloc (mangle_type_decl_to_module (`Suffix "zipper") type_decl))
        [
          thunk;
          wrap_decl ~is_derivative:true derivative;
          wrap_decl ~is_derivative:false ancestor;
          wrap_decl ~is_derivative:false zipper;
          wrap_decl ~is_derivative:false view;
          Code_gen.zip decl;
          Code_gen.go_up decl derivative;
          Code_gen.unzip decl;
          Code_gen.view decl;
        ]
    ]
  | _ -> assert false

let () = Ppx_deriving.(register (create ~type_decl_str "zipper" ()))
