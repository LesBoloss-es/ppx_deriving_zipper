open Ztype

(** {2 Substitution of holes with flat types} *)

let rec subst_flat t = function
  | Hole -> t
  | Var v -> Var v
  | Product terms -> Product (List.map (subst_flat t) terms)
  | Constr (name, args) -> Constr (name, List.map (subst_flat t) args)

let subst_constructor t (cons, args) =
  let args = List.map (subst_flat t) args in
  cons, args

let subst t = function
  | Union constructors -> Union (List.map (subst_constructor t) constructors)
  | Flat fl -> Flat (subst_flat t fl)

(** {2 Names handling} *)

let guess_derivative_name _name = "diff"
let guess_ancestor_name _name = "ancestor"
let guess_zipper_name _name = "t"
let guess_view_name _name = "view"
let guess_view_constr_name name = "Z" ^ name

(** {2 Type generation} *)

let derivative {vars; name; def} =
  {
    name = guess_derivative_name name;
    def = Derive.typ name def;
    vars;
  }

let ancestor {vars; name; _} =
  let args = unit :: List.map (fun v -> Var v) vars in
  let def = Flat (Constr (guess_derivative_name name, args)) in
  {name = guess_ancestor_name name; vars = vars; def}

let zipper {name; vars; _} =
  let args = List.map (fun v -> Var v) vars in
  let ancestors = Constr ("list", [Constr (guess_ancestor_name name, args)]) in
  let typ = Product [Constr (name, args); ancestors] in
  {vars; name = guess_zipper_name name; def = Flat typ}

let view {name; vars; def} =
  let def = match def with
    | Flat _ -> def
    | Union variants ->
      let variants = List.map
        (fun (constr, args) ->
           let name = guess_view_constr_name (constr_name constr) in
           let constr = {name; kind = FromCons (constr.name, args)} in
           (constr, args))
        variants
      in
      Union variants
  in
  let subst = Constr ("thunk", [Constr (guess_zipper_name name, List.map var_ vars)]) in
  let def = Ztype.replace_constr name subst def in
  let name = guess_view_name name in
  {name; vars; def}

let all decl =
  let derivative = derivative decl in
  let ancestor = ancestor decl in
  let zipper = zipper decl in
  derivative, ancestor, zipper
