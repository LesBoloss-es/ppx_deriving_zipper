let rec monomial (x : string) : Types.monomial -> Types.monomial list
  = function
    | Var y when y = x -> [Hole]
    | Var _ -> []
    | Product ms -> monomial_product x [] ms
    | App (name, args) ->
        List.mapi
          (fun i arg ->
            List.map
              (fun m ->
                Types.Product [App (Naming.d name i, args); m])
              (monomial x arg))
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
    @
    (* if we derive something later... *)
    monomial_product x (m :: acc) ms

(* FIXME: different constructor names? *)
let rec polynomial (x : string) : Types.polynomial -> Types.polynomial
  = function
  | [] -> []
  | (c, m) :: ms ->
    List.map (fun m' -> (c, m')) (monomial x m)
    @ polynomial x ms
