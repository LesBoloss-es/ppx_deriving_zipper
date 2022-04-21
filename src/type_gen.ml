let type_gen (td : Types.decl) : Syntax.type_declaration list =
  let (Fixpoint (poly, fix_var)) = td.def in

  let app_to_vars name =
    Types.App (name, List.map Types.var_ td.vars)
  in
  let constr_to_vars name =
    Syntax.Constr (name, List.map Syntax.var_ td.vars)
  in

  let poly_zdfix =
    let poly' = Derive.polynomial fix_var poly in
    let poly' = Types.substitute_polynomial poly'
        ~var:fix_var ~by:(app_to_vars td.name)
    in
    Syntax.
      { name= Naming.poly_zdfix td.name
      ; vars= td.vars
      ; definition= Variant (Types.Print.polynomial poly')
      ; loc= Location.none }
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
          Constr ("list", [constr_to_vars poly_zdfix.name])
        ]);
      loc = Location.none
    }
  in

  [poly_zdfix]
  @ poly_zdvars
  @ [zdz]
  (* let dis = assert false in *)
  (* dis *)
