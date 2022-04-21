let type_gen (td : Types.decl) : Syntax.type_declaration list =
  let (Fixpoint (poly, fix_var)) = td.def in
  let by =
    Types.App (td.name, List.map Types.var_ td.vars)
  in

  let poly_zdfix =
    let poly' = Derive.polynomial fix_var poly in
    let poly' = Types.substitute_polynomial ~var:fix_var ~by poly' in
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
         let poly' = Types.substitute_polynomial ~var:fix_var ~by poly' in
         Syntax.
           { name= Naming.poly_zd td.name var
           ; vars= td.vars
           ; definition= Variant (Types.Print.polynomial poly')
           ; loc= Location.none })
    td.vars
  in
  let ancestor =
    Syntax.{
      name = Naming.ancestor td.name;
      vars = td.vars;
      definition = Alias (Constr (poly_zdfix.name, List.map Syntax.var_ td.vars));
      loc = Location.none;
    }
  in

  [poly_zdfix]
  @ poly_zdvars
  @ [ancestor]
  (* let zdz = assert false in *)
  (* let dis = assert false in *)
  (* let ancestor = assert false in *)
  (* let zipper = assert false in *)
  (* [poly_zdfix] @ poly_zdvars @ [zdz] @ dis @ [ancestor; zipper] *)
