let named_monomial x (constructor_name, m) =
  List.mapi
    (fun i term -> (Naming.nth_constructor constructor_name i, term))
    (Monomial.derive x m)

(** [polynomial x poly] computes the derivative of the polynomial [poly]
    with respect to the variable [x] *)
let rec polynomial (x : string) : Polynomial.polynomial -> Polynomial.polynomial =
  function
  | [] -> []
  | m :: ms -> named_monomial x m @ polynomial x ms


(** TODO: move following function to [Polynomial] and link to doc about
    pseudo-derivation in [Monomial]. *)

(** WARNING: here we treat [poly] as if it was divided by [z]. Put differently,
    we treat the contructors as constants rather than [z]s. *)
let polynomial_pseudo ~var poly =
  List.concat_map
    (fun (constr, mono) ->
      List.map
        (fun (path, mono) ->
          Naming.head_constructor constr (Monomial.Path.to_list path), mono)
        (Monomial.derive_pseudo ~var Monomial.Path.empty mono))
    poly
