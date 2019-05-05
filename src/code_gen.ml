open Ztype
open Parsetree
open Ast_helper

let zip decl =
  let fun_name = Pat.var ("zip_" ^ decl.name |> Location.mknoloc) in
  let loc = Location.none in
  [%stri let [%p fun_name] = fun t -> t, []]
