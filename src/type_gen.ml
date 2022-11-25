(** The current type applied to its type variables. *)
let the_type (td : Types.decl) : Types.monomial =
  Types.App (td.name, List.map Types.var_ td.vars)

(** Generate the partial derivative of the polynomial of a type (eg. tree_d0,
    tree_d1, etc). *)
let poly_zdvar (td : Types.decl) (var : string) : _ =
  let (Fixpoint (poly, fix_var)) = td.def in

  poly
  |> Derive.polynomial var
  |> Types.substitute_polynomial ~var:fix_var ~by:(the_type td)

(** Generate the [ancestors] type *)
let ancestors (td : Types.decl) : Types.decl =
  let (Fixpoint (poly, fix_var)) = td.def in
  let fix_var2 = "fixme_var2" in

  let poly' =
    poly
    |> Derive.polynomial fix_var
    |> Types.substitute_polynomial ~var:fix_var ~by:(the_type td)
    |> Types.polynomial_flat_right_multiply_by_monomial (Var fix_var2)
    |> Types.polynomial_add ["NoAncestor", Types.one]
  in
  Types.{
    name = Naming.ancestors td.name;
    vars = td.vars;
    def = Fixpoint (poly', fix_var2);
  }

let type_gen (td : Types.decl) : Syntax.type_declaration list =
  let constr_to_vars name =
    Syntax.Constr (name, List.map Syntax.var_ td.vars)
  in

  let ancestors = Types.Print.decl (ancestors td) in

  let poly_zdvars =
    List.map
      (fun var ->
        let poly = poly_zdvar td var in
        let definition = Syntax.Variant (Types.Print.polynomial poly) in
        Syntax.
          { name= Naming.poly_zd td.name var
          ; vars= td.vars
          ; recursive= false
          ; definition
          ; loc= Location.none })
      td.vars
  in

  let zdz =
    Syntax.{
      name = Naming.zdz td.name;
      vars = td.vars;
      recursive = false;
      definition = Alias (Product [
          constr_to_vars td.name;
          constr_to_vars ancestors.name
        ]);
      loc = Location.none
    }
  in

  let dis =
    List.mapi
      (fun i poly_zdvar ->
        Syntax.{
          name = Naming.d td.name i;
          vars = td.vars;
          recursive = false;
          definition = Alias (Product [
              constr_to_vars poly_zdvar.name;
              constr_to_vars ancestors.name
            ]);
          loc = Location.none
        })
      poly_zdvars
  in

  [ancestors]
  @ poly_zdvars
  @ [zdz]
  @ dis
