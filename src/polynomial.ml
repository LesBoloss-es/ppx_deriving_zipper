(** {1 Polynomials} *)

(** A polynomial is a list of monomials, tagged with constructor names. *)
type t = (string * Monomial.t) list
[@@deriving show {with_path = false}]

(** {2 Variable occurrence and substitution} *)

(** Return [true] if and only if the variable [var] occurs in the polynomial. *)
let occurs ~var =
  List.exists (fun (_, mono) -> Monomial.occurs ~var mono)

(** Substitution in polynomials *)
let substitute ~var ~by =
  List.map
    (fun (cname, m) ->
       (cname, Monomial.substitute ~var ~by m))

(** {2 Arithmetic operations on polynomials} *)

let add = (@)

let flat_right_multiply_by_monomial mono' =
  List.map (fun (cname, mono) -> (cname, Monomial.flat_multiply mono mono'))

(** {2 Partial derivations} *)

(** [derive x poly] computes the derivative of the polynomial [poly]
    with respect to the variable [x] *)
let derive (x : string) : t -> t =
  List.concat_map
    (fun (constructor_name, m) ->
      List.mapi
        (fun i term ->
          (Naming.nth_constructor constructor_name i, term))
        (Monomial.derive x m))
