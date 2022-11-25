(** {2 Types as fixpoints of polynomials} *)

type fixpoint = Fixpoint of Polynomial.t * string
[@@deriving show {with_path = false}]

type decl = {
  name: string;
  vars: string list;
  def: fixpoint
}
[@@deriving show {with_path = false}]
(** Example:
  The type
  {[
  type 'a bintree =
    | Leaf of 'a
    | Node of 'a bintree * 'a bintree
  ]}
  yields:
  {[
    {
      name = "bintree";
      vars = ["a"];
      def = Fixpoint (
          [("Leaf", Var "a"); ("Node", Product [Var "b"; Var "b"])],
          "b"
        );
    }
  ]}
*)

(** {3 From Syntax} *)

module Parse = struct
  (** [naive_subs ~loc name vars fix_var name' args'] *)
  (** - name: name of the type (eg. t) *)
  (** - vars: variables of the type (eg. 'a) *)
  (** - fix_var: type variable that will replace eg. ['a t] *)
  (** - name': name of the type encountered within the definition (eg. s) *)
  (** - args': type arguments of the the type encountered with the definition (eg. ['b], [int]) *)
  let rec naive_subs ~loc name vars fix_var
    : string -> Syntax.core_type list -> Monomial.t
    =
    fun name' args' ->
      if name = name' then
        let vars = List.map Syntax.var_ vars in
        if args' = vars then Var fix_var
        else
          Location.raise_errorf ~loc
            "Unsupported: use of same type with different variables: %s" name
      else
        let subs = naive_subs ~loc name vars fix_var in
        let args' = List.map (monomial_with_subs subs) args' in
        App (name', args')

  and monomial_with_subs subs : Syntax.core_type -> Monomial.t =
    function
    | Var name -> Var name
    | Constr (name, args) -> subs name args
    | Product args -> Monomial.product (List.map (monomial_with_subs subs) args)

  let polynomial_with_subs subs =
    List.map
      (fun (constr_name, args) ->
        (constr_name, Monomial.product (List.map (monomial_with_subs subs) args)))

  let decl (td: Syntax.type_declaration) : decl =
    let fix_var = "fixpoint" (* FIXME *) in
    let subs = naive_subs ~loc:td.loc td.name td.vars fix_var in
    match td.definition with
    | Variant variants ->
      {
        name = td.name;
        vars = td.vars;
        def = Fixpoint (polynomial_with_subs subs variants, fix_var)
      }
    | Alias _ ->
      Format.eprintf "TODO: implement zippers of type aliases@.";
      assert false
end

(** {2 To Syntax} *)

module Print = struct
  let rec monomial: Monomial.t -> _ = function
    | Var x -> Syntax.Var x
    | Prod ms -> Product (List.map monomial ms)
    | App (name, args) -> Constr (name, List.map monomial args)
    | Hole -> Constr ("hole", []) (* FIXME: fully-qualified name *)

  let polynomial p =
    List.map
      (fun (c, m) ->
         match monomial m with
         | Product ms -> (c, ms)
         | m -> (c, [m]))
      p

  let decl {name; vars; def} =
    let Fixpoint (poly, fix_var) = def in
    let type_app = Monomial.App (name, List.map Monomial.var vars) in
    let poly = Polynomial.substitute poly ~var:fix_var ~by:type_app in
    Syntax.{
      name;
      vars;
      recursive = true;
      definition = Variant (polynomial poly);
      loc = Location.none;
    }
end
