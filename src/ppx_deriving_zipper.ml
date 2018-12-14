open Parsetree
open Longident

let rec core_type_occurs ~name core_type =
  match core_type.ptyp_desc with
  (* | Ptyp_any | Ptyp_var _ -> false *)
  (* | Ptyp_arrow (_, t1, t2) -> core_type_occurs ~name t1 || core_type_occurs ~name t2
   * | Ptyp_tuple ts -> List.exists (core_type_occurs ~name) ts *)
  | Ptyp_constr (i, ts) -> longident_occurs ~name i.txt || List.exists (core_type_occurs ~name) ts
  (* | Ptyp_object _ -> assert false
   * | Ptyp_class _ -> assert false
   * | Ptyp_alias (t, _) -> core_type_occurs ~name t
   * | Ptyp_variant (_fs, _, _) -> (\* List.exists (row_field_occurs ~name) fs *\) assert false
   * | Ptyp_poly (_, _t) -> (\* core_type_occurs ~name t *\) assert false
   * | Ptyp_package _ -> assert false
   * | Ptyp_extension _ -> assert false *)
  | _ -> assert false

(* and row_field_occurs ~name = function
 *   | Rtag (_, _, _, ts) -> List.exists (core_type_occurs ~name) ts
 *   | Rinherit t -> core_type_occurs ~name t *)

and longident_occurs ~name = function
  | Lident name' -> name = name'
  | _ -> assert false

let type_decl_str ~options ~path =
  ignore options; ignore path; function
  | [type_decl] ->
     assert (List.length type_decl.ptype_cstrs = 0);
     assert (type_decl.ptype_manifest = None);
     (
       match type_decl.ptype_kind with
       | Ptype_variant constr_decls ->
          (
            let (present, absent) =
              List.partition
                (fun constr_decl ->
                  match constr_decl.pcd_args with
                  | Pcstr_tuple ts -> List.exists (core_type_occurs ~name:type_decl.ptype_name.txt) ts
                  | _ -> assert false)
                constr_decls
            in
            List.iter
              (fun constr_decl ->
                print_endline constr_decl.pcd_name.txt)
              present;
            print_newline ();

            List.iter
              (fun constr_decl ->
                print_endline constr_decl.pcd_name.txt)
              absent;
            print_newline ();

            print_int (List.length constr_decls)
          )
       | _ -> failwith "not a variant"
     );
     Obj.magic ()
  | _ -> assert false

let () = Ppx_deriving.(register (create ~type_decl_str "zipper" ()))
