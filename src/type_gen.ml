let type_gen (td : Types.decl) : Syntax.type_declaration list =
  let (Fixpoint (poly, fix_var)) = td.def in

  let app_to_vars name =
    Types.App (name, List.map Types.var_ td.vars)
  in
  let constr_to_vars name =
    Syntax.Constr (name, List.map Syntax.var_ td.vars)
  in

  let fix_var2 = "fixme_var2" in

  let ancestors =
    let poly' = Derive.polynomial fix_var poly in
    let poly' = Types.substitute_polynomial poly'
        ~var:fix_var ~by:(app_to_vars td.name)
    in
    let poly' =
      poly'
      |> Types.polynomial_flat_multiply_by_monomial (Var fix_var2)
      |> Types.polynomial_add ["Nil", Types.one]
    in
    Types.(Print.decl {
        name = Naming.ancestors td.name;
        vars = td.vars;
        def = Fixpoint (poly', fix_var2);
      })
  in

  let poly_zdvars =
    List.map
      (fun var ->
         let poly' = Derive.polynomial var poly in
         let poly' = Types.substitute_polynomial poly'
             ~var:fix_var ~by:(app_to_vars td.name)
         in
         Syntax.
           { name= Naming.poly_zd td.name var
           ; vars= td.vars
           ; definition= Variant (Types.Print.polynomial poly')
           ; loc= Location.none })
      td.vars
  in

  let zdz =
    Syntax.{
      name = Naming.zdz td.name;
      vars = td.vars;
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
