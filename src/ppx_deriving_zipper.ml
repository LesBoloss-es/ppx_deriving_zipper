open Ast_helper
open Parsetree
open Longident

let lid id = Location.mknoloc (Longident.Lident id)
let str s = Location.mknoloc s

(** default location used by metaquot when building ast nodes with [%expr ...] *)
let loc = Location.none

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

let typ_unit = [%type: unit]

let guess_name constr_name i =
  constr_name ^ "_" ^ (string_of_int i)

let make_constr name core_types =
  Type.constructor
    (Location.mknoloc name)
    ~args:(Pcstr_tuple core_types)

let derive_tuple ty_name args =
  (* Example: for [ty_name * int * ty_name], this function returns
     [0, unit * int * ty_name;
      2, ty_name * int * unit] *)
  (* FIXME: broken on tuples containing tuples (correction: broken on almost
     everything!) *)
  ExtList.find_all_indices (core_type_occurs ~name:ty_name) args
  |> List.map (fun i -> (i, ExtList.replace_nth args i typ_unit))

let%test _ =
  let t = [%type: t] in
  let int = [%type: int] in
  let unit = [%type: unit] in
  derive_tuple "t" [t; int; t] = [(0, [unit; int; t]); (2, [t; int; unit])]

let derive_constr type_name constr_decl =
  match constr_decl.pcd_args with
  | Pcstr_tuple args ->
    derive_tuple type_name args
    |> List.map (fun (i, args) ->
        let original_name = constr_decl.pcd_name.txt in
        let name = guess_name original_name i in
        (name, i, original_name, make_constr name args))

  | Pcstr_record _ -> failwith "no support for inline records"

let generate_constructors type_decl =
  let type_name = type_decl.ptype_name.txt in
  match type_decl.ptype_kind with
  | Ptype_variant constr_decls -> ExtList.flat_map (derive_constr type_name) constr_decls
  | Ptype_record _ -> assert false
  | Ptype_abstract -> assert false
  | Ptype_open -> assert false

let generate_ancestor type_decl =
  let type_name = type_decl.ptype_name.txt in
  match type_decl.ptype_kind with
  | Ptype_variant constr_decls ->
    let ancestor_constrs =
      ExtList.flat_map (derive_constr type_name) constr_decls
    in
    let ancestor =
      Type.mk
        ~kind:(Ptype_variant (List.map (fun (_, _, _, x) -> x) ancestor_constrs))
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

let generate_to_zipper type_decl =
  let type_name = type_decl.ptype_name.txt in
  let fun_name = Pat.var ("zip_" ^ type_name |> Location.mknoloc) in
  [[%stri let [%p fun_name] = fun t -> t, []]]

let generate_go_up type_decl constructors =
  let type_name = type_decl.ptype_name.txt in
  let generate_match_case (name, i, original_name, constr_args) =
    let nb_args = match constr_args.pcd_args with
      | Pcstr_tuple l -> List.length l
      | Pcstr_record _ -> assert false
    in
    let tuple_pattern =
      let make_var j =
        if j <> i then Pat.var (str (Format.sprintf "x%d" j))
        else [%pat? ()]
      in
      List.init nb_args make_var
    in
    let pattern = Pat.construct (lid name) (Some (Pat.tuple tuple_pattern)) in
    let constructor_args =
      let make_arg j =
        if j <> i then Exp.ident (lid (Format.sprintf "x%d" j))
        else [%expr t]
      in
      List.init nb_args make_arg
    in
    let body = Exp.construct (lid original_name) (Some (Exp.tuple constructor_args)) in
    {
      pc_lhs = pattern;
      pc_guard = None;
      pc_rhs = body;
    }

  in
  let value =
    let fun_name = type_name ^ "_go_up" in
    let ancestor_match = Exp.match_
        [%expr ancestor]
        (List.map generate_match_case constructors)
    in
    Vb.mk (Pat.var (fun_name |> Location.mknoloc))
      [%expr fun (t, ancestors) -> match ancestors with
        | [] -> invalid_arg [%e Exp.constant (Const.string fun_name)]
        | ancestor :: ancestors ->
          let tree = [%e ancestor_match] in
          tree, ancestors
      ]

  in
  [Str.value Asttypes.Nonrecursive [value]]

let type_decl_str ~options ~path =
  ignore options;
  ignore path;
  function
  | [type_decl] ->
    assert (List.length type_decl.ptype_cstrs = 0);
    (match type_decl.ptype_manifest with
     | None -> ()
     | Some core_type -> print_endline (Ppx_deriving.string_of_core_type core_type));
    let constructors = generate_constructors type_decl in
    generate_ancestor type_decl
    @ generate_zipper type_decl
    @ generate_to_zipper type_decl
    @ generate_go_up type_decl constructors
  | _ -> assert false

let () = Ppx_deriving.(register (create ~type_decl_str "zipper" ()))
