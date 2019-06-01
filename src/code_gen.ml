open Ztype
open Parsetree
open Ast_helper

let loc = Location.none

let zip typ =
  let fun_name = Pat.var ("zip_" ^ typ.name |> Location.mknoloc) in
  let loc = Location.none in
  [%stri let [%p fun_name] = fun t -> t, []]

let map_with_name f =
  List.mapi
    (fun j arg -> f (Format.sprintf "x%d" j) arg)

let go_up typ derivative =
  let generate_match_case (cons, args) =
    let name = constr_name cons in
    let orig_name = match cons.kind with
      | FromCons (orig_name, _) -> orig_name
      | _ -> assert false
    in
    let tuple_pattern =
      map_with_name
        (fun var_name arg ->
           if arg = Hole then [%pat? ()]
           else Pat.var (str var_name))
        args
    in
    let pattern = Pat.construct (lid name) (Some (Pat.tuple tuple_pattern)) in
    let constructor_args =
      map_with_name
        (fun var_name arg ->
           if arg = Hole then [%expr t]
           else Exp.ident (lid var_name))
        args
    in
    let body = Exp.construct (lid orig_name) (Some (Exp.tuple constructor_args)) in
    {
      pc_lhs = pattern;
      pc_guard = None;
      pc_rhs = body;
    }

  in
  let value =
    let fun_name = "go_up_" ^ typ.name in
    let ancestor_match = Exp.match_
        [%expr derivative]
        (List.map generate_match_case (variants derivative.def))
    in
    Vb.mk (Pat.var (fun_name |> Location.mknoloc))
      [%expr fun (t, ancestors) -> match ancestors with
        | [] -> invalid_arg [%e Exp.constant (Const.string fun_name)]
        | derivative :: ancestors ->
          let tree = [%e ancestor_match] in
          tree, ancestors
      ]

  in
  Str.value Asttypes.Nonrecursive [value]
