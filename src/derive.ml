(** [monomial x m] computes the derivative of the monomial [m] with respect to
    the variable [x].
    Note that deriving a monomial may yield a sum of monomials
    (eg. [d(u * v)/dx = du/dx * v + u * dv/dx]. *)
let rec monomial (x : string) : Monomial.monomial -> Monomial.monomial list = function
  | Var y when y = x -> [Hole]
  | Var _ -> []
  | Product ms -> monomial_product x [] ms
  | App (name, args) ->
    List.mapi
      (fun i arg ->
        List.map
          (fun m -> Monomial.Product [App (Naming.d name i, args); m])
          (monomial x arg) )
      args
    |> List.flatten
  | Hole -> assert false

and monomial_product x acc = function
  | [] -> []
  | m :: ms ->
    (* if we derive this [m] (as [m'])... *)
    List.map
      (fun m' -> Monomial.product (List.rev_append acc (m' :: ms)))
      (monomial x m)
    @ (* if we derive something later... *)
    monomial_product x (m :: acc) ms

let named_monomial x (constructor_name, m) =
  List.mapi
    (fun i term -> (Naming.nth_constructor constructor_name i, term))
    (monomial x m)

(** [polynomial x poly] computes the derivative of the polynomial [poly]
    with respect to the variable [x] *)
let rec polynomial (x : string) : Polynomial.polynomial -> Polynomial.polynomial =
  function
  | [] -> []
  | m :: ms -> named_monomial x m @ polynomial x ms

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

let rec monomial_pseudo ~var (path : Path.t) :
    Monomial.monomial -> (Path.t * Monomial.monomial) list = function
  | Var _ -> []
  | Product ms -> monomial_product_pseudo ~var 0 path [] ms
  | App (name, args) as m ->
    if Monomial.occurs_monomial var m then
      monomial_app_pseudo ~var path name args
    else
      []
  | Hole -> assert false

and monomial_product_pseudo ~var i path acc = function
  | [] -> []
  | m :: ms ->
    (* if we derive this [m] (as [m'])... *)
    List.map
      (fun (path', m') ->
        (path', Monomial.product (List.rev_append acc (m' :: ms))))
      (monomial_pseudo ~var (Path.enter_product i path) m)
    @ (* if we derive something later... *)
    monomial_product_pseudo ~var (i + 1) path (m :: acc) ms

and monomial_app_pseudo ~var path name args =
  List.mapi
    (fun i arg ->
      List.map
        (fun (path', m') ->
          (path', Monomial.Product [App (Naming.d name i, args); m']))
        (monomial_pseudo ~var (Path.enter_app i path) arg) )
    args
  |> List.flatten
  |> List.cons (path, Monomial.App (Naming.zdz name, args))

(** WARNING: here we treat [poly] as if it was divided by [z]. Put differently,
    we treat the contructors as constants rather than [z]s. *)
let polynomial_pseudo ~var poly =
  List.concat_map
    (fun (constr, mono) ->
      List.map
        (fun (path, mono) ->
          Naming.head_constructor constr (Path.to_list path), mono)
        (monomial_pseudo ~var Path.empty mono))
    poly
