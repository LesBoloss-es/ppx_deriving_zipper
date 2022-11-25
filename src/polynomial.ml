(** {2 Polynomials} *)

(** List of [(constructor name, monomial)] *)
type polynomial = (string * Monomial.t) list
[@@deriving show {with_path = false}]

(** Substitution in polynomials *)
let substitute_polynomial ~var ~by =
  List.map
    (fun (cname, m) ->
       (cname, Monomial.substitute ~var ~by m))

let occurs_polynomial var =
  List.exists (fun (_, mono) -> Monomial.occurs ~var mono)

(** {3 Arithmetic operations on polynomials} *)

let polynomial_add = (@)

let polynomial_flat_right_multiply_by_monomial mono' =
  List.map (fun (cname, mono) -> (cname, Monomial.monomial_flat_multiply mono mono'))
