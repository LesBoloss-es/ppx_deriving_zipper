open Ppxlib.Parsetree
open Ppxlib.Ast_helper
open More_ast_helpers

(** Given a type [foo], generate a function of type [foo -> foo_zipper] that
  constructs the zipper pointing at the root of the structure. *)
let zip (td : Types.decl) : _ =
  let loc = Location.none in
  let type_variables = List.map (Typ.var ~loc) td.vars in
  let type_annotation = Typ.constr ~loc (lid td.name) type_variables in
  let zipper_annotation =
    Typ.constr ~loc (lid (Naming.zdz td.name)) type_variables
  in
  [%stri
    let [%p Pat.var (str (Naming.zip td.name))] =
     fun (x : [%t type_annotation]) : [%t zipper_annotation] -> (x, Nil)]
