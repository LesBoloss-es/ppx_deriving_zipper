(** Various helpers for computing type derivatives *)

open Ztype

(** {2 Differentiation of flat types} *)

let constr v name = function
  | [] when v = name -> [Hole]
  | [] -> []
  | _ -> assert false

(** The derivative of a product with respect to a type variable *)
let rec product v terms =
  (* XXX. not efficient but readable *)
  let derive_i i =
    flat v (List.nth terms i)
    |> List.map (fun t -> ExtList.replace_nth terms i t)
  in
  List.init (List.length terms) derive_i
  |> List.flatten

(* FIXME: [v] peut être une variable ou un constructeur *)
(** The derivative of a flat type with respect to a type variable *)
and flat v = function
  | Var v' when v = v' -> [Hole]
  | Var _ -> []
  | Product terms -> List.map (fun x -> Product x) (product v terms)
  | Constr (name, args) -> constr v name args
  | Hole -> invalid_arg "flat"

let%test _ =
  let t = Constr ("t", []) in
  let int = Constr ("int", []) in
  product "t" [t; int; t] = [[Hole; int; t]; [t; int; Hole]]


(** {2 Differentiation of union types} *)

let guess_name i name = Format.sprintf "%s_%i" name i

(** The derivative of a single constructor of a union type with respect to a
    type variable *)
let derive_constructor v (constr, args) =
  let original_name = constr_name constr in
  let variants = product v args in
  List.mapi
    (fun i t ->
       let name = guess_name i original_name in
       let kind = FromCons (original_name, args) in
       ({name; kind}, t))
    variants

(** The derivative of a union type with respect to a type variable *)
let union v constructors =
  ExtList.flat_map (derive_constructor v) constructors

(** The derivative of a tyep with respect to a type variable *)
let typ v = function
  | Union constructors -> Union (union v constructors)
  | Flat fl ->
    let ts = flat v fl in
    let make_constr i t =
      let name = guess_name i "FromFlat" in
      let kind = FromFlat fl in
      ({name; kind}, [t])
    in
    Union (List.mapi make_constr ts)
