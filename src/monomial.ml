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

(** {2 Arithmetic operations on monomials} *)

(** Multiply but flattens one level of product. *)
let monomial_flat_multiply m1 m2 =
  match (m1, m2) with
  | Prod m1s, Prod m2s -> Prod (m1s @ m2s)
  | Prod m1s, _ -> Prod (m1s @ [m2])
  | _, Prod m2s -> Prod (m1 :: m2s)
  | _, _ -> Prod [m1; m2]

(** {2 Partial derivations} *)

(** [derive x m] computes the derivative of the monomial [m] with respect to
    the variable [x].
    Note that deriving a monomial typically yields a sum of monomials because of
    the Leibniz rule ([d(u * v)/dx = du/dx * v + u * dv/dx]) and each term of
    the sum is contains exactly one occurrence of the symbol [Hole] which
    represents where a variable have be killed by the derivation. *)
let rec derive (x : string) : t -> t list = function
  | Var y when y = x -> [Hole]
  | Var _ -> []
  | Prod ms -> derive_prod x [] ms
  | App (name, args) -> derive_app x name args
  | Hole -> assert false

and derive_prod x acc = function
  | [] -> []
  | m :: ms ->
    (* if we derive this [m] (as [m'])... *)
    List.map
      (fun m' -> product (List.rev_append acc (m' :: ms)))
      (derive x m)
    @ (* if we derive something later... *)
    derive_prod x (m :: acc) ms

and derive_app x name args =
  args
  |> List.mapi
      (fun i arg ->
        List.map
          (fun m -> Prod [App (Naming.d name i, args); m])
          (derive x arg) )
  |> List.flatten
