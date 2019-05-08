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

let guess_derivative_name name = name ^ "_diff"
let guess_ancestor_name name = name ^ "_ancestor"
let guess_zipper_name name = name ^ "_zipper"

(** {2 Type generation} *)

let derivative {vars; name; def} =
  let d = Derive.typ name def in
  {
    name = guess_derivative_name name;
    vars = "hole" :: vars;
    def = subst (Var "hole") d
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

let all decl =
  let derivative = derivative decl in
  let ancestor = ancestor decl in
  let zipper = zipper decl in
  derivative, ancestor, zipper
