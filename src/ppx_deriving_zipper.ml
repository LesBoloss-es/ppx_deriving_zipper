open Ast_helper
open Parsetree
open Longident

let lid id = Location.mknoloc (Longident.Lident id)

let longident_occurs ~name = function
  | Lident name' -> name = name'
  | _ -> assert false

let rec core_type_occurs ~name core_type =
  match core_type.ptyp_desc with
  | Ptyp_any | Ptyp_var _ -> false
  | Ptyp_arrow (_, t1, t2) -> core_type_occurs ~name t1 || core_type_occurs ~name t2
  | Ptyp_tuple ts -> List.exists (core_type_occurs ~name) ts
  | Ptyp_constr (i, ts) -> longident_occurs ~name i.txt || List.exists (core_type_occurs ~name) ts
  | Ptyp_object _ -> assert false
  | Ptyp_class _ -> assert false
  | Ptyp_alias (t, _) -> core_type_occurs ~name t
  | Ptyp_variant (_fs, _, _) -> (* List.exists (row_field_occurs ~name) fs *) assert false
  | Ptyp_poly (_, _t) -> (* core_type_occurs ~name t *) assert false
  | Ptyp_package _ -> assert false
  | Ptyp_extension _ -> assert false

and row_field_occurs ~name = function
  | Rtag (_, _, _, ts) -> List.exists (core_type_occurs ~name) ts
  | Rinherit t -> core_type_occurs ~name t

let typ_unit = Typ.constr (lid "unit") []

let make_constr name cpt core_types =
  Type.constructor
    (Location.mknoloc (name ^ string_of_int cpt))
    ~args:(Pcstr_tuple core_types)

let rec derive_tuple ty_name cpt prefix acc = function
  | [] -> acc
  | arg :: args ->
     let acc =
       if core_type_occurs ~name:ty_name arg then
         (cpt, List.rev_append prefix (typ_unit :: args)) :: acc
       else
         acc
     in
     derive_tuple ty_name (cpt + 1) (arg :: prefix) acc args

let derive_tuple ty_name args =
  derive_tuple ty_name 0 [] [] args

let derive_constr type_name constr_decl =
  match constr_decl.pcd_args with
  | Pcstr_tuple args ->
     derive_tuple type_name args
     |> List.map (fun (i, args) ->
            make_constr constr_decl.pcd_name.txt i args)
  | Pcstr_record _ -> assert false

let generate_ancestor type_decl =
  let type_name = type_decl.ptype_name.txt in
  match type_decl.ptype_kind with
  | Ptype_variant constr_decls ->
     let ancestor_constrs =
       List.map (derive_constr type_name) constr_decls
       |> List.concat
     in
     let ancestor =
       Type.mk
         ~kind:(Ptype_variant ancestor_constrs)
         (Location.mknoloc (type_name ^ "_ancestor"))
     in
     [Str.type_ Asttypes.Recursive [ancestor]]

  | Ptype_record label_decls ->
     List.iter
       (fun label_decl ->
         print_endline label_decl.pld_name.txt)
       label_decls;
     failwith "not a variant"

  | Ptype_abstract -> assert false
  | Ptype_open -> assert false

let generate_zipper type_decl =
  let type_name = type_decl.ptype_name.txt in
  [Str.type_ Asttypes.Recursive [
       Type.mk
         ~manifest:(Typ.tuple [
                        Typ.constr (lid type_name) [];
                        Typ.constr (lid "list") [Typ.constr (lid (type_name ^ "_ancestor")) []]])
         (Location.mknoloc (type_name ^ "_zipper"))]]

let type_decl_str ~options ~path =
  ignore options; ignore path; function
  | [type_decl] ->
     assert (List.length type_decl.ptype_cstrs = 0);
     (match type_decl.ptype_manifest with
      | None -> ()
      | Some core_type -> print_endline (Ppx_deriving.string_of_core_type core_type));
     generate_ancestor type_decl
     @ generate_zipper type_decl
  | _ -> assert false

let () = Ppx_deriving.(register (create ~type_decl_str "zipper" ()))
