(** Various helpers for computing type derivatives *)

open Ztype

(** {2 Differentiation of flat types} *)

let occurrences_of_name name =
  Position.collect
    (function
      | Constr (name', _) when name = name' -> true
      | _ -> false)

(** Derive a flat type *)
let flat v flat =
  List.map
    (fun path -> path, Position.replace_at path Hole flat)
    (occurrences_of_name v flat)

(** FIXME: I'm cheum *)
let product v flats =
  flat v (Product flats)
  |> List.map (fun (pos, typ) -> pos, destruct_product typ)

let%test _ =
  let t = Constr ("t", []) in
  let int = Constr ("int", []) in
  let flats = [Product [int; t]; t] in
  let v = "t" in
  let actual = product v flats  |> List.map snd |> List.sort compare in
  let expected = List.sort compare [[Product [int; Hole]; t]; [Product [int; t]; Hole]] in
  actual = expected


(** {2 Differentiation of union types} *)

(* FIXME *)
let guess_name pos name = Format.sprintf "%s_%i" name (List.hd pos)

(** The derivative of a single constructor of a union type with respect to a
    type variable *)
let constructor v (constr, args) =
  let original_name = constr_name constr in
  let variants = product v args in
  List.map
    (fun (pos, typ) ->
       let name = guess_name pos original_name in
       let kind = FromCons (original_name, args) in
       ({name; kind}, typ))
    variants

(** The derivative of a union type with respect to a type variable *)
let union v constructors =
  ExtList.flat_map (constructor v) constructors

(** The derivative of a type with respect to a type variable *)
let typ v = function
  | Union constructors -> Union (union v constructors)
  | Flat fl ->
    let ts = flat v fl in
    let make_constr (pos, typ) =
      let name = guess_name pos "FromFlat" in
      let kind = FromFlat fl in
      ({name; kind}, [typ])
    in
    Union (List.map make_constr ts)
