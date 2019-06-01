(** {2 Our simplified representation of types} *)

(** "Flat" types: anything but a union type *)
type flat =
  | Var of string
  | Product of flat list
  | Constr of string * flat list
  (** for instance: [int], [int list] or [(string, int) Hashtbl.t] *)
  | Hole
  (** a special symbol used when computing derivatives *)

let var_ v = Var v

(** A general type is either a flat type or a union type.
    Defined this way, unions cannot be nested inside other types *)
type t =
  | Flat of flat
  | Union of (constructor * flat list) list

(** Data constructors *)
and constructor = {name: string; kind: cons_kind}

and cons_kind =
  | User
  | FromCons of string * flat list
  | FromFlat of flat

(** Type declarations: a type name and a definition *)
type decl = {vars: string list; name: string; def: t}

(** {2 Basic operations on types} *)

let rec replace_constr_flat name value = function
  | Var v -> Var v
  | Product typs -> Product (List.map (replace_constr_flat name value) typs)
  | Constr (name', args) ->
    if name = name' then begin
      assert (args = []); (* FIXME *)
      value
    end else
    Constr (name', List.map (replace_constr_flat name value) args)
  | Hole -> Hole

let replace_constr name value = function
  | Flat t -> Flat (replace_constr_flat name value t)
  | Union variants ->
    let variants = List.map (fun (c, args) -> (c, List.map (replace_constr_flat name value) args)) variants in
    Union variants

let unit = Constr ("unit", [])

let mk_constr name = {name; kind = User}
let constr_name (c: constructor) = c.name


(** {2 Pretty printing (for debugging purpose only)} *)

let pp_constructor fmt c =
  Format.pp_print_string fmt (constr_name c)

let rec pp_flat fmt = function
  | Var name -> Format.fprintf fmt "'%s" name
  | Product typs ->
    let pp_sep fmt () = Format.fprintf fmt " * " in
    Format.fprintf fmt "(%a)" (Format.pp_print_list ~pp_sep pp_flat) typs
  | Constr (name, args) ->
    let pp_sep fmt () = Format.fprintf fmt ", " in
    let pp_args = Format.pp_print_list ~pp_sep pp_flat in
    begin match args with
      | [] -> Format.pp_print_string fmt name
      | [arg] -> Format.fprintf fmt "%a %s" pp_flat arg name
      | _ -> Format.fprintf fmt "(%a) %s" pp_args args name
    end
  | Hole -> Format.fprintf fmt "?"

let pp fmt = function
  | Flat t -> pp_flat fmt t
  | Union variants ->
    let pp_variant fmt (constructor, args) =
      let pp_sep fmt () = Format.fprintf fmt " * " in
      match args with
      | [] -> pp_constructor fmt constructor
      | _ ->
        Format.fprintf fmt "%a of %a"
          pp_constructor constructor
          (Format.pp_print_list ~pp_sep pp_flat) args
    in
    let pp_sep fmt () = Format.fprintf fmt "@\n| " in
    Format.pp_print_list ~pp_sep pp_variant fmt variants

let pp_vars fmt vars =
  let pp_sep fmt () = Format.fprintf fmt ", " in
  let pp_var fmt = Format.fprintf fmt "'%s" in
  match vars with
  | [] -> ()
  | [v] -> pp_var fmt v
  | _ -> Format.pp_print_list ~pp_sep pp_var fmt vars

let pp_decl fmt {vars; name; def} =
  Format.fprintf fmt "@[<2>type %a %s =@\n%a@]" pp_vars vars name pp def

(** {2 Conversion} *)

(** {3 From Parsetree} *)

let unsupported s =
  Format.eprintf "Unsupported: %s@." s;
  exit 1

let flat_of_core_type _ = assert false

let flat_of_constr_arg ct =
  let open Parsetree in
  match ct.ptyp_desc with
  | Ptyp_var name -> Var name
  | Ptyp_constr (name, args) ->
    let name = Longident.flatten name.txt |> String.concat "." in
    Constr (name, List.map flat_of_core_type args)

  | Ptyp_any -> unsupported "Ptyp_any"
  | Ptyp_arrow _ -> unsupported "Ptyp_arrow"
  | Ptyp_tuple _ -> unsupported "Ptyp_tuple"
  | Ptyp_object _ -> unsupported "Ptyp_object"
  | Ptyp_class _ -> unsupported "Ptyp_class"
  | Ptyp_alias _ -> unsupported "Ptyp_alias"
  | Ptyp_variant _ -> unsupported "Ptyp_variant"
  | Ptyp_poly _ -> unsupported "Ptyp_poly"
  | Ptyp_package _ -> unsupported "Ptyp_package"
  | Ptyp_extension _ -> unsupported "Ptyp_extension"

let variant_of_constr_decl cd =
  let open Parsetree in
  assert (cd.pcd_res = None); (* what is this? *)
  let constructor = mk_constr cd.pcd_name.txt in
  match cd.pcd_args with
  | Pcstr_tuple args -> (constructor, List.map flat_of_constr_arg args)
  | Pcstr_record _ -> unsupported "Pcstr_record"

let decl_of_type_declaration td =
  let open Parsetree in
  let name = td.ptype_name.txt in
  let as_var ct = match ct.ptyp_desc with
    | Ptyp_var v -> v
    | _ -> invalid_arg "as_var"
  in
  match td.ptype_kind with
  | Ptype_variant constr_decls ->
    let variants = List.map variant_of_constr_decl constr_decls in
    let vars = List.map (fun (v, _) -> as_var v) td.ptype_params in
    {vars; name; def = Union variants}
  | Ptype_record _ -> unsupported "Ptype_record"
  | Ptype_abstract -> unsupported "Ptype_abstract"
  | Ptype_open -> unsupported "Ptype_open"

(** {3 To Parsetree} *)

let str = Location.mknoloc
let lid s = Longident.parse s |> Location.mknoloc

let rec flat_to_type flat =
  let open Ast_helper in
  match flat with
  | Var name -> Typ.var name
  | Constr (name, args) -> Typ.constr (lid name) (List.map flat_to_type args)
  | Product terms -> Typ.tuple (List.map flat_to_type terms)
  | Hole -> invalid_arg "flat_to_type"

let variables_to_params =
  let open Ast_helper in
  List.map (fun v -> Typ.var v, Asttypes.Invariant)

let to_decl {vars; name; def} =
  let open Parsetree in
  let open Ast_helper in
  let params = variables_to_params vars in
  match def with
  | Flat fl ->
    let manifest = flat_to_type fl in
    let kind = Ptype_abstract in
    Type.mk ~params ~kind ~manifest (str name)
  | Union variants ->
    let mk_constructor (c, args) =
      let name = Location.mknoloc (constr_name c) in
      let args = Pcstr_tuple (List.map flat_to_type args) in
      Type.constructor ~args name
    in
    let kind = Ptype_variant (List.map mk_constructor variants) in
    Type.mk ~params ~kind (str name)
