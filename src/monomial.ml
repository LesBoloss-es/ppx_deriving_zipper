(** {1 Monomials} *)

(** A monomial is any type expression that does not involve sums. *)
type t =
  | Var of string
      (** Type variable. FIXME: maybe replace this [string] with
      [string | Fix] to have a cleaner handling of the fixpoint variable? *)
  | Prod of t list
      (** A product/tuple type.
      Invariant 1: must never have one element.
      Invariant 2: the empty product is only allowed to appear as a term in a
      polynomial, not as a sub-expression of another monomial. *)
  | App of string * t list
      (** Type application (eg. [('a t, int) Hashtbl.t]) *)
  | Hole
      (** Type hole created by a derivation.
      FIXME: Hole -> Ppx_deriving_zipper.hole? *)
[@@deriving show {with_path = false}]

(** {2 Constants and curryfied constructors} *)

(** The type [1] (not unit).
    It represents the absence of data constructor argument in a sum type. *)
let one = Prod []

let var x = Var x

(** Create a product from a list of types.
    The [Product] constructor is omitted when the list contains one element. *)
let product = function
  | [] -> one
  | [x] -> x
  | args -> Prod args

let app name args = App (name, args)

(** {2 Variable occurrence and substitution} *)

(** Return [true] if and only if the variable [var] occurs in the monomial. *)
let rec occurs ~var = function
  | Var var' -> var = var'
  | Hole -> false
  | Prod ms | App (_, ms) -> List.exists (occurs ~var) ms

(** Substitution in monomials *)
let rec substitute ~var ~by = function
  | Var var' when var = var' -> by
  | (Var _ | Hole) as m -> m
  | Prod ms -> Prod (List.map (substitute ~var ~by) ms)
  | App (name, ms) -> App (name, List.map (substitute ~var ~by) ms)

(** {3 Arithmetic operations on monomials} *)

(** Multiply but flattens one level of product. *)
let monomial_flat_multiply m1 m2 =
  match (m1, m2) with
  | Prod m1s, Prod m2s -> Prod (m1s @ m2s)
  | Prod m1s, _ -> Prod (m1s @ [m2])
  | _, Prod m2s -> Prod (m1 :: m2s)
  | _, _ -> Prod [m1; m2]
