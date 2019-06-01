open Ztype
open Parsetree
open Ast_helper

let loc = Location.none

let zip typ =
  let fun_name = Pat.var ("zip_" ^ typ.name |> Location.mknoloc) in
  let loc = Location.none in
  [%stri let [%p fun_name] = fun t -> t, []]

let var_name_from_pos = Format.sprintf "x%d"
let exp_var_from_pos i = Exp.ident (lid (var_name_from_pos i))
let pat_var_from_pos i = Pat.var (str (var_name_from_pos i))

let make_constructor_pattern constr_name constr_args mk_arg =
  let args = match constr_args with
    | [] -> None
    | _ -> Some (Pat.tuple (List.mapi mk_arg constr_args)) in
  Pat.construct (lid constr_name) args

let make_constructor_expr constr_name constr_args mk_arg =
  let args = match constr_args with
    | [] -> None
    | _ -> Some (Exp.tuple (List.mapi mk_arg constr_args)) in
  Exp.construct (lid constr_name) args

let go_up typ derivative =
  let generate_match_case (cons, args) =
    let pattern =
      let make_arg pos arg = if arg = Hole then [%pat? ()] else pat_var_from_pos pos in
      make_constructor_pattern (constr_name cons) args make_arg
    in
    let body =
      let make_arg pos arg = if arg = Hole then [%expr t] else exp_var_from_pos pos in
      make_constructor_expr (original_constructor_name cons) args make_arg
    in
    Exp.case pattern body
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
          tree, ancestors]
  in
  Str.value Asttypes.Nonrecursive [value]

let view typ =
  let fun_name = Pat.var ("view_" ^ typ.name |> Location.mknoloc) in
  let loc = Location.none in
  let match_cases: case list =
    List.map
      (fun (constr, constr_args) ->
         let constr_name = constr_name constr in
         let pattern =
           make_constructor_pattern
             constr_name
             constr_args
             (fun pos _ -> pat_var_from_pos pos)
         in
         let body =
           let ugly_index = ref (-1) in
           make_constructor_expr
             (Type_gen.guess_view_constr_name constr_name)
             constr_args
             (fun pos arg ->
                match arg with
                | Constr (name, _) when typ.name = name ->
                  incr ugly_index;
                  let child = exp_var_from_pos pos in
                  let new_ancestor =
                    make_constructor_expr
                      (Derive.guess_name !ugly_index constr_name)
                      constr_args
                      (fun pos' _ ->
                         if pos = pos' then [%expr ()]
                         else exp_var_from_pos pos')
                  in
                  [%expr fun () -> ([%e child], [%e new_ancestor] :: ancestors)]
                | _ -> exp_var_from_pos pos )
         in
         Exp.case pattern body
      )
      (variants typ.def)
  in
  let big_match = Exp.match_ [%expr tree] match_cases in
  (* FIXME: empty list? *)
  let zipper_name = Typ.constr (lid (Type_gen.guess_zipper_name typ.name)) [] in
  (* FIXME: empty list? *)
  let view_name = Typ.constr (lid (Type_gen.guess_view_name typ.name)) [] in
  [%stri let [%p fun_name] : [%t zipper_name] -> [%t view_name] = fun (tree, ancestors) -> [%e big_match]]

let unzip typ =
  let fun_pat = Pat.var ("unzip_" ^ typ.name |> Location.mknoloc) in
  let fun_expr = Exp.ident ("unzip_" ^ typ.name |> lid) in
  let go_up_name = Exp.ident ("go_up_" ^ typ.name |> lid) in
  [%stri let rec [%p fun_pat] = fun zipper ->
    match snd zipper with
    | [] -> fst zipper
    | _ -> [%e fun_expr] ([%e go_up_name] zipper)]
