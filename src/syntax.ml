(** {1 Syntax} *)

open Ppxlib

(** Simplified view of [Stdlib.Parsetree]. *)

type core_type =
  | Var of string
  | Product of core_type list
  | Constr of string * core_type list
  (** for instance: [int], [int list] or [(string, int) Hashtbl.t] *)

let var_ name = Var name

type definition =
  | Variant of (string * core_type list) list
    (** eg. [[Foo of int * bool; Bar of 'a]] *)
  | Alias of core_type
    (** eg. [int * bool list] *)

(** Type declarations: a type name and a definition *)
type type_declaration = {
  name : string;      (** type name, eg. [t] *)
  vars : string list; (** type variables, eg ['a] *)
  recursive : bool;
  definition : definition;
  loc: Location.t
}

(** {3 From Parsetree} *)

module Parse = struct
  let unsupported s =
    Format.eprintf "Unsupported: %s@." s;
    exit 1

  let rec core_type (ct : Parsetree.core_type) : core_type =
    let open Parsetree in
    match ct.ptyp_desc with
    | Ptyp_var name -> Var name
    | Ptyp_constr (name, args) ->
      let name = Longident.flatten_exn name.txt |> String.concat "." in
      Constr (name, List.map core_type args)
    | Ptyp_any -> unsupported "Ptyp_any"
    | Ptyp_arrow _ -> unsupported "Ptyp_arrow"
    | Ptyp_tuple args -> Product (List.map core_type args)
    | Ptyp_object _ -> unsupported "Ptyp_object"
    | Ptyp_class _ -> unsupported "Ptyp_class"
    | Ptyp_alias _ -> unsupported "Ptyp_alias"
    | Ptyp_variant _ -> unsupported "Ptyp_variant"
    | Ptyp_poly _ -> unsupported "Ptyp_poly"
    | Ptyp_package _ -> unsupported "Ptyp_package"
    | Ptyp_extension _ -> unsupported "Ptyp_extension"

  let variant (cd : Parsetree.constructor_declaration) : string * core_type list =
    let open Parsetree in
    assert (cd.pcd_res = None); (* FIXME: what is this? *)
    let constructor = cd.pcd_name.txt in
    match cd.pcd_args with
    | Pcstr_tuple args -> (constructor, List.map core_type args)
    | Pcstr_record _ -> unsupported "Pcstr_record"

  let type_declaration td =
    let open Parsetree in
    let name = td.ptype_name.txt in
    let as_var ct = match ct.ptyp_desc with
      | Ptyp_var v -> v
      | _ -> invalid_arg "as_var"
    in
    match td.ptype_kind with
    | Ptype_variant constr_decls ->
      let definition = Variant (List.map variant constr_decls) in
      let vars = List.map (fun (v, _) -> as_var v) td.ptype_params in
      { name; vars; recursive = false; definition; loc = td.ptype_loc }
    | Ptype_record _ -> unsupported "Ptype_record"
    | Ptype_abstract -> unsupported "Ptype_abstract"
    | Ptype_open -> unsupported "Ptype_open"
end

(** {3 Our AST Helpers} *)

let str txt = Location.{ txt; loc = none }

let lid s =
  Location.{ txt = Longident.parse s; loc = none }

(** {3 To Parsetree} *)

module Print = struct
  let rec core_type =
    let open Ast_helper in
    function
    | Var name -> Typ.var name
    | Constr (name, args) -> Typ.constr (lid name) (List.map core_type args)
    | Product terms -> Typ.tuple (List.map core_type terms)

  let mk_constructor (c, args) =
    let open Ast_helper in
    let name = str c in
    let args = Pcstr_tuple (List.map core_type args) in
    Type.constructor ~args name

  let kind_and_manifest = function
    | Variant variants ->
      Ptype_variant (List.map mk_constructor variants), None
    | Alias ct ->
      Ptype_abstract, Some (core_type ct)

  let type_declaration {name; vars; recursive; definition; loc} =
    let open Ast_helper in
    let params =
      List.map
        (fun v -> Typ.var v, (Asttypes.NoVariance, Asttypes.NoInjectivity))
        vars
    in
    let kind, manifest = kind_and_manifest definition in
    Str.type_
      (if recursive then Recursive else Nonrecursive)
      [Type.mk ~loc ~params ~kind ?manifest (str name)]
end
