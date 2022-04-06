type monomial =
  | Var of string
  | Product of monomial list
  | Hole
  (* FIXME: Hole -> Ppx_deriving_zipper.hole? *)
  (* FIXME: what about [('a list)]? *)
  [@@deriving show {with_path = false}]

let product = function
  | [] -> invalid_arg "Types.product"
  | [x] -> x
  | args -> Product args

type polynomial = (string * monomial) list
[@@deriving show {with_path = false}]

type fixpoint = Fixpoint of polynomial * string
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
  let naive_subs ~loc name vars fix_var
    : string -> Syntax.core_type list -> monomial
  =
    let vars = List.map Syntax.var_ vars in
    fun name' args ->
      if name = name' then
        if args = vars then Var fix_var
        else
          Location.raise_errorf ~loc
            "Unsupported: use of same type with different variables: %s" name
      else
        Location.raise_errorf ~loc
          "Unsupported: use of a type that is not %s" name

  let rec monomial_with_subs subs : Syntax.core_type -> monomial =
    function
    | Var name -> Var name
    | Constr (name, args) -> subs name args
    | Product args -> product (List.map (monomial_with_subs subs) args)

  let polynomial_with_subs subs =
    List.map
      (fun (constr_name, args) ->
        (constr_name, product (List.map (monomial_with_subs subs) args)))

  let decl (td: Syntax.type_declaration) : decl =
    let fix_var = "fixpoint" (* FIXME *) in
    let subs = naive_subs ~loc:td.loc td.name td.vars fix_var in
    {
      name = td.name;
      vars = td.vars;
      def = Fixpoint (polynomial_with_subs subs td.variants, fix_var)
    }
end
