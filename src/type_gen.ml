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
  let poly_zdvars =
    List.map
      (fun var ->
         let poly' = Derive.polynomial var poly in
         Syntax.
           { name= Naming.poly_zd td.name var
           ; vars= td.vars
           ; definition= Variant (Types.Print.polynomial poly')
           ; loc= Location.none })
    td.vars
  in
  [poly_zdfix]
  @ poly_zdvars
  (* let zdz = assert false in *)
  (* let dis = assert false in *)
  (* let ancestor = assert false in *)
  (* let zipper = assert false in *)
  (* [poly_zdfix] @ poly_zdvars @ [zdz] @ dis @ [ancestor; zipper] *)
