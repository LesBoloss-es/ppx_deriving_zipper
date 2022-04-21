let type_gen (td : Types.decl) : Syntax.type_declaration list =
  let (Fixpoint (poly, fix_var)) = td.def in
  let poly_zdfix =
    let poly' = Derive.polynomial fix_var poly in
    Syntax.
      { name= Naming.poly_zdfix td.name
      ; vars= td.vars
      ; definition= Variant (Types.Print.polynomial poly')
      ; loc= Location.none }
  in
  [poly_zdfix]
  (* let poly_zdvars = assert false in *)
  (* let zdz = assert false in *)
  (* let dis = assert false in *)
  (* let ancestor = assert false in *)
  (* let zipper = assert false in *)
  (* [poly_zdfix] @ poly_zdvars @ [zdz] @ dis @ [ancestor; zipper] *)
