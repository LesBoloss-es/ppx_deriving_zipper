(* open Ast_helper *)
(* open Parsetree *)

(* let wrap_decl ?(non_rec=true) ~is_derivative d = *)
(*   Str.type_ *)
(*     Asttypes.(if non_rec then Nonrecursive else Recursive) *)
(*     [Ztype.to_decl ~is_derivative d] *)

(* let mangle_type_decl_to_module ?(fixpoint="t") affix type_ = *)
(*   let cap = String.capitalize_ascii in *)
(*   (match affix with *)
(*    | `Prefix prefix | `PrefixSuffix (prefix, _) -> cap prefix *)
(*    | _ -> "") *)
(*   ^ *)
(*   (let type_ = type_.ptype_name.txt in *)
(*    if type_ = fixpoint then "" else cap type_) *)
(*   ^ *)
(*   (match affix with *)
(*    | `Suffix suffix | `PrefixSuffix (_, suffix) -> cap suffix *)
(*    | _ -> "") *)

let handle_type_decl type_decl =
  Format.eprintf "%a\n;;@."
    Pprintast.structure_item (Ast_helper.Str.type_ Asttypes.Nonrecursive [type_decl]);
  let syntax = Syntax.Parse.type_declaration type_decl in
  let decl = Types.Parse.decl syntax in
  Format.eprintf "%a@." Types.pp_decl decl;
  (
    let Types.Fixpoint (p, fix_var) = decl.def in
    List.iter
      (fun x ->
         let p' = Derive.polynomial x p in
         let decl_lol =
           Syntax.{ name = "derivative_"^x; vars = []; loc = Location.none;
                    variants = Types.Print.polynomial p' }
           |> Syntax.Print.type_declaration
         in
         Format.eprintf "%a\n%a@."
           Types.pp_polynomial p'
           Pprintast.structure_item (Ast_helper.Str.type_ Asttypes.Nonrecursive [decl_lol])
      )
      (fix_var :: decl.vars)
  );
  exit 0

let type_decl_str ~options ~path =
  ignore options;
  ignore path;
  function
  | [type_decl] -> handle_type_decl type_decl
  | _ -> assert false

  (*   (\* sanity checks / debugging *\) *)
  (*   assert (List.length type_decl.ptype_cstrs = 0); *)
  (*   (match type_decl.ptype_manifest with *)
  (*    | None -> () *)
  (*    | Some core_type -> print_endline (Ppx_deriving.string_of_core_type core_type)); *)

  (*   let decl = Ztype.decl_of_type_declaration type_decl in *)
  (*   let derivative, ancestor, zipper = Type_gen.all decl in *)
  (*   let view = Type_gen.view decl in *)
  (*   let thunk = *)
  (*     let loc = Location.none in *)
  (*     [%stri type 'a thunk = unit -> 'a] *)
  (*   in *)
  (*   let in_module name str = Str.module_ (Mb.mk name (Mod.structure str)) in *)
  (*   [ *)
  (*     in_module *)
  (*       (Location.mknoloc (mangle_type_decl_to_module (`Suffix "zipper") type_decl)) *)
  (*       [ *)
  (*         thunk; *)
  (*         wrap_decl ~is_derivative:true derivative; *)
  (*         wrap_decl ~is_derivative:false ancestor; *)
  (*         wrap_decl ~is_derivative:false zipper; *)
  (*         wrap_decl ~is_derivative:false view; *)
  (*         Code_gen.zip decl; *)
  (*         Code_gen.go_up decl derivative; *)
  (*         Code_gen.unzip decl; *)
  (*         Code_gen.view decl; *)
  (*       ] *)
  (*   ] *)
  (* | _ -> assert false *)

let () = Ppx_deriving.(register (create ~type_decl_str "zipper" ()))
