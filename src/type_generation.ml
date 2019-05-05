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

let all {name; def} =
  let d = Derive.typ name def in
  let ancestor = {name = guess_ancestor_name name; def = subst unit d} in
  let zipper = {name = guess_zipper_name name; def = assert false} in
  ancestor, zipper
