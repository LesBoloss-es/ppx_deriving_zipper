(** [monomial x m] computes the derivative of the monomial [m] with respect to
    the variable [x].
    Note that deriving a monomial may yield a sum of monomials
    (eg. [d(u * v)/dx = du/dx * v + u * dv/dx]. *)
let rec monomial (x : string) : Types.monomial -> Types.monomial list = function
  | Var y when y = x -> [Hole]
  | Var _ -> []
  | Product ms -> monomial_product x [] ms
  | App (name, args) ->
    List.mapi
      (fun i arg ->
        List.map
          (fun m -> Types.Product [App (Naming.d name i, args); m])
          (monomial x arg) )
      args
    |> List.flatten
  | Hole -> assert false

and monomial_product x acc = function
  | [] -> []
  | m :: ms ->
    (* if we derive this [m] (as [m'])... *)
    List.map
      (fun m' -> Types.product (List.rev_append acc (m' :: ms)))
      (monomial x m)
    @ (* if we derive something later... *)
    monomial_product x (m :: acc) ms

let named_monomial x (constructor_name, m) =
  List.mapi
    (fun i term -> (Naming.nth_constructor constructor_name i, term))
    (monomial x m)

(** [polynomial x poly] computes the derivative of the polynomial [poly]
    with respect to the variable [x] *)
let rec polynomial (x : string) : Types.polynomial -> Types.polynomial =
  function
  | [] -> []
  | m :: ms -> named_monomial x m @ polynomial x ms
