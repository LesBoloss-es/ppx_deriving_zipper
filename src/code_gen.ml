open Ztype
open Parsetree
open Ast_helper

let loc = Location.none

let zip typ =
  let fun_name = Pat.var ("zip_" ^ typ.name |> Location.mknoloc) in
  let loc = Location.none in
  [%stri let [%p fun_name] = fun t -> t, []]

let go_up typ derivative =
  let constructors = match derivative.def with
    | Union constructors -> constructors
    | Flat _ -> assert false
  in
  let generate_match_case (cons, args) =
    let name = constr_name cons in
    let orig_name = match cons.kind with
      | FromCons (orig_name, _) -> orig_name
      | _ -> assert false
    in
    let tuple_pattern =
      List.mapi
        (fun j arg ->
           if arg <> unit then Pat.var (str (Format.sprintf "x%d" j))
           else [%pat? ()])
        args
    in
    let pattern = Pat.construct (lid name) (Some (Pat.tuple tuple_pattern)) in
    let constructor_args =
      List.mapi
        (fun j arg ->
           if arg <> unit then Exp.ident (lid (Format.sprintf "x%d" j))
           else [%expr t])
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
        (List.map generate_match_case constructors)
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
