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
let product terms = Product terms
let constr name args = Constr (name, args)

let destruct_product = function
  | Product args -> args
  | _ -> invalid_arg "destruct_product"

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

let variants = function
  | Flat _ -> invalid_arg "variants"
  | Union variants -> variants

let original_constructor_name {kind; _} = match kind with
  | FromCons (name, _) -> name
  | _ -> invalid_arg "original_constructor_name"

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

module Position = struct
  type t = int list

  (** All the positions in the type such that [p substree] holds *)
  let collect p =
    let rec visit pos acc flat =
      let acc = if p flat then List.rev pos :: acc else acc in
      match flat with
      | Hole | Var _ -> acc
      | Product terms -> visit_list pos acc terms
      | Constr (_, args) -> visit_list pos acc args
    and visit_list pos acc types =
      let visit_i (acc, i) typ = visit (i :: pos) acc typ, i + 1 in
      let acc, _ = List.fold_left visit_i (acc, 0) types in
      acc
    in
    visit [] []

  (** Substitute the subtree at pos [pos] with [value] *)
  let rec replace_at pos value flat = match pos with
    | [] -> value
    | i :: pos ->
      match flat with
      | Product terms ->
        List.nth terms i
        |> replace_at pos value
        |> ExtList.replace_nth terms i
        |> product
      | Constr (name, args) ->
        List.nth args i
        |> replace_at pos value
        |> ExtList.replace_nth args i
        |> constr name
      | Hole | Var _ -> invalid_arg "replace_at"

let map_pos ~on_var ~on_hole ~on_product ~on_constr flat =
  let rec visit pos = function
    | Var v -> on_var (List.rev pos) v
    | Hole -> on_hole (List.rev pos)
    | Product terms -> List.mapi (fun i term -> visit (i :: pos) term) terms |> on_product (List.rev pos)
    | Constr (name, args) -> List.mapi (fun i term -> visit (i :: pos) term) args |> on_constr (List.rev pos) name
  in
  visit [] flat
end


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

