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
let flat_multiply m1 m2 =
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

(** {2 Pseudo derivation wrt z, focusing on fix var} *)

(** These functions are used to define the [head] type.

    Consider a type defined inductively by:

      t(a0, a1, ..., an) = P(z, t, a0, a1, ..., an)

    The initial intuition is that we should derive P with respect to z so as to
    point at any constructor occurring in P (but not in an inner t).
    The problem with that is that if something like [int list] occurs in P, then
    all of the constructors of this list are eligible to pointing.

    We thus want to filter out expressions that do not depend on t.
    A way to implement this is to use the following operator (FIXME: I think
    this is called a pseudo-derivation):

    D(expr(z, t, a0, a1, ..., an)) =
      z * d/dz (expr(z, t, a0, a1, ..., an) - expr(z, 0, a0, a1, ..., an))

    (Note the evaluation at t=0 in the second term).
    This does exactly what we want: ignore the parts that do not depend on t
    and, derive the rest.

    Note: in the implementation, we do not perform a subtraction, we filter out
    the expression that do not depend on t on the fly.
 *)

module Path : sig
  type t

  val empty: t
  val enter_product: int -> t -> t
  val enter_app: int -> t -> t
  val to_list: t -> [`Product of int | `App of int] list
end = struct
  type step = [`App of int | `Product of int]
  type t = step list
  (** Invariant: reversed *)

  let empty = []
  let enter_product i path = `Product i :: path
  let enter_app i path = `App i :: path
  let to_list = List.rev
end

let rec derive_pseudo ~var (path : Path.t) : t -> (Path.t * t) list = function
  | Var _ -> []
  | Prod ms -> derive_prod_pseudo ~var 0 path [] ms
  | App (name, args) as m ->
    if occurs ~var m then derive_app_pseudo ~var path name args else []
  | Hole -> assert false

and derive_prod_pseudo ~var i path acc = function
  | [] -> []
  | m :: ms ->
    (* if we derive this [m] (as [m'])... *)
    List.map
      (fun (path', m') ->
        (path', product (List.rev_append acc (m' :: ms))))
      (derive_pseudo ~var (Path.enter_product i path) m)
    @ (* if we derive something later... *)
    derive_prod_pseudo ~var (i + 1) path (m :: acc) ms

and derive_app_pseudo ~var path name args =
  List.mapi
    (fun i arg ->
       List.map
         (fun (path', m') ->
            (path', Prod [App (Naming.d name i, args); m']))
         (derive_pseudo ~var (Path.enter_app i path) arg) )
    args
  |> List.flatten
  |> List.cons (path, App (Naming.zdz name, args))
