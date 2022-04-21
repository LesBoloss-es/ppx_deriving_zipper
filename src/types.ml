type monomial =
  | Var of string
  | Product of monomial list
  | App of string * monomial list    (** type application (eg. [('a t, int) Hashtbl.t]) *)
  | Hole
  (* FIXME: Hole -> Ppx_deriving_zipper.hole? *)
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
  (** [naive_subs ~loc name vars fix_var name' args'] *)
  (** - name: name of the type (eg. t) *)
  (** - vars: variables of the type (eg. 'a) *)
  (** - fix_var: type variable that will replace eg. ['a t] *)
  (** - name': name of the type encountered within the definition (eg. s) *)
  (** - args': type arguments of the the type encountered with the definition (eg. ['b], [int]) *)
  let rec naive_subs ~loc name vars fix_var
    : string -> Syntax.core_type list -> monomial
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

  and monomial_with_subs subs : Syntax.core_type -> monomial =
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

(** {3 To Syntax} *)

module Print = struct
  let rec monomial = function
    | Var x -> Syntax.Var x
    | Product ms -> Product (List.map monomial ms)
    | App (name, args) -> Constr (name, List.map monomial args)
    | Hole -> Constr ("hole", []) (* FIXME: fully-qualified name *)

  let polynomial p =
    List.map
      (fun (c, m) ->
         match monomial m with
         | Product ms -> (c, ms)
         | m -> (c, [m]))
      p
end
