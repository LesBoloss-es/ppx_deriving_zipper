open Ztype
open Parsetree
open Ast_helper

let loc = Location.none

(** {2 Names handling} *)

let guess_zip_name _name = "zip"
let guess_unzip_name _name = "unzip"
let guess_go_up_name _name = "go_up"
let guess_view_name _name = "view"

(** {2} Helpers *)

let var_name_from_pos =
  let pp_int_list =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "_")
      Format.pp_print_int
  in
  Format.asprintf "x_%a" pp_int_list

let exp_var_from_pos pos = Exp.ident (lid (var_name_from_pos pos))
let pat_var_from_pos pos = Pat.var (str (var_name_from_pos pos))

let make_constructor_pattern
    ?(on_hole = fun pos -> pat_var_from_pos pos)
    ?(on_constr = fun pos _ _ -> pat_var_from_pos pos)
    ?(on_var = fun pos _ -> pat_var_from_pos pos)
    constr_name constr_args =
  let args = match constr_args with
    | [] -> None
    | _ ->
      let pattern = Position.map_pos
          ~on_product:(fun _ terms -> Pat.tuple terms)
          ~on_var
          ~on_constr
          ~on_hole
          (Product constr_args)
      in
      Some pattern
  in
  Pat.construct (lid constr_name) args

let make_constructor_expr
    ?(on_hole = fun pos -> exp_var_from_pos pos)
    ?(on_constr = fun pos _ _ -> exp_var_from_pos pos)
    ?(on_var = fun pos _ -> exp_var_from_pos pos)
    constr_name constr_args =
  let args = match constr_args with
    | [] -> None
    | _ ->
      let expr = Position.map_pos
          ~on_product:(fun _ terms -> Exp.tuple terms)
          ~on_var
          ~on_hole
          ~on_constr
          (Product constr_args)
      in
      Some expr
  in
  Exp.construct (lid constr_name) args

(** {2 Code generation} *)

let zip typ =
  let fun_name = Pat.var (guess_zip_name typ.name |> Location.mknoloc) in
  let loc = Location.none in
  [%stri let [%p fun_name] = fun t -> t, []]

let go_up typ derivative =
  let generate_match_case (cons, args) =
    Exp.case
      (make_constructor_pattern ~on_hole:(fun _ -> [%pat? ()]) (constr_name cons) args)
      (make_constructor_expr ~on_hole:(fun _ -> [%expr t]) (original_constructor_name cons) args)
  in
  let value =
    let fun_name = guess_go_up_name typ.name in
    let ancestor_match = Exp.match_
        [%expr derivative]
        (List.map generate_match_case (variants derivative.def))
    in
    Vb.mk (Pat.var (fun_name |> Location.mknoloc))
      [%expr fun (t, ancestors) -> match ancestors with
        | [] -> None
        | derivative :: ancestors ->
          let tree = [%e ancestor_match] in
          Some (tree, ancestors)]
  in
  Str.value Asttypes.Nonrecursive [value]

let view typ =
  let fun_name = Pat.var (guess_view_name typ.name |> Location.mknoloc) in
  let loc = Location.none in
  let match_cases: case list =
    List.map
      (fun (constr, constr_args) ->
         let on_hole _ = failwith "view/hole" in
         let constr_name = constr_name constr in
         let pattern = make_constructor_pattern constr_name constr_args ~on_hole in
         let body =
           make_constructor_expr
             (Type_gen.guess_view_constr_name constr_name)
             constr_args
             ~on_hole
             ~on_constr:(fun pos name args ->
                 if name = typ.name then begin
                   assert (args = []); (* FIXME *)
                   let child = exp_var_from_pos pos in
                   let new_ancestor =
                     make_constructor_expr
                       (Derive.guess_name pos constr_name)
                       constr_args
                       ~on_constr:(fun pos' _ _ ->
                           if pos = pos' then [%expr ()]
                           else exp_var_from_pos pos')
                   in
                   [%expr fun () -> ([%e child], [%e new_ancestor] :: ancestors)]
                 end else
                   exp_var_from_pos pos
               )
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
  let unzip_name = guess_unzip_name typ.name in
  let fun_pat = Pat.var (Location.mknoloc unzip_name) in
  let fun_expr = Exp.ident (lid unzip_name) in
  let go_up_name = Exp.ident (guess_go_up_name typ.name |> lid) in
  [%stri let rec [%p fun_pat] = fun zipper ->
      match [%e go_up_name] zipper with
      | None -> fst zipper
      | Some zipper -> [%e fun_expr] zipper]
