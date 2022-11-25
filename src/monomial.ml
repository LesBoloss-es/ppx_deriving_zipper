(** {2 Monomials} *)

type monomial =
  | Var of string  (* FIXME: replace string with string | Fix ? *)
  | Product of monomial list (* always at least two elements *)
  | App of string * monomial list    (** type application (eg. [('a t, int) Hashtbl.t]) *)
  | Hole
  (* FIXME: Hole -> Ppx_deriving_zipper.hole? *)
  [@@deriving show {with_path = false}]

let var_ x = Var x
let one = Product []

(** Substitution in monomials *)
let rec substitute_monomial ~var ~by = function
  | Var var' when var = var' -> by
  | (Var _ | Hole) as m -> m
  | Product ms -> Product (List.map (substitute_monomial ~var ~by) ms)
  | App (name, ms) -> App (name, List.map (substitute_monomial ~var ~by) ms)

let rec occurs_monomial var = function
  | Var var' -> var = var'
  | Hole -> false
  | Product ms | App (_, ms) -> List.exists (occurs_monomial var) ms

(** {3 Arithmetic operations on monomials} *)

let product = function
  | [] -> one
  | [x] -> x
  | args -> Product args

(** Multiply but flattens one level of product. *)
let monomial_flat_multiply m1 m2 =
  match (m1, m2) with
  | Product m1s, Product m2s -> Product (m1s @ m2s)
  | Product m1s, _ -> Product (m1s @ [m2])
  | _, Product m2s -> Product (m1 :: m2s)
  | _, _ -> Product [m1; m2]
