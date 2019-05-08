open Ztype

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

let guess_ancestor_name name = name ^ "_ancestor"
let guess_zipper_name name = name ^ "_zipper"

let ancestor {name; def} =
  let d = Derive.typ name def in
  {name = guess_ancestor_name name; def = subst unit d}

let zipper type_name ancestor_name =
  let name = guess_zipper_name type_name in
  let ancestors = Constr ("list", [Constr (ancestor_name, [])]) in
  let typ = Product [Constr (type_name, []); ancestors] in
  {name; def = Flat typ}

let all typ =
  let ancestor = ancestor typ in
  let zipper = zipper typ.name ancestor.name in
  ancestor, zipper
