open Ast_helper
open Parsetree
open Longident


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

let typ_unit = Typ.constr (Lident "unit" |> Location.mknoloc) []

let make_constr name cpt core_type = name, cpt, core_type

let rec derive_constr ty_name constr_name cpt prefix acc = function
  | [] -> acc
  | arg :: args ->
    let acc =
      if core_type_occurs ~name:ty_name arg then
        make_constr constr_name cpt (List.rev_append prefix (typ_unit :: args)) :: acc
      else
        acc
    in
    derive_constr ty_name constr_name (cpt + 1) (arg :: prefix) acc args
let derive_constr ty_name constr_name args = derive_constr ty_name constr_name 0 [] [] args

let print_constr fmt (name, cpt, _) =
  Format.fprintf fmt "%s%d" name cpt

let type_decl_str ~options ~path =
  ignore options; ignore path; function
    | [type_decl] ->
      assert (List.length type_decl.ptype_cstrs = 0);
      assert (type_decl.ptype_manifest = None);
      (
        match type_decl.ptype_kind with
        | Ptype_variant constr_decls ->
          (
            let zipper_constrs =
              List.map (
                  fun constr_decl -> match constr_decl.pcd_args with
                  | Pcstr_tuple args -> derive_constr type_decl.ptype_name.txt constr_decl.pcd_name.txt args
                  | Pcstr_record _ -> assert false
                ) constr_decls
              |> List.concat
            in
            List.iter (Format.printf "%a@." print_constr) zipper_constrs
          )
        | _ -> failwith "not a variant"
      );
      Obj.magic ()
    | _ -> assert false

let () = Ppx_deriving.(register (create ~type_decl_str "zipper" ()))
