  $ ../standalone.exe ./bintree_list.ml
  [DEBUG] type ('a, 'b) bintree =
    | Leaf of 'a list 
    | Node of 'b * ('a, 'b) bintree * ('a, 'b) bintree [@@deriving zipper]
  [DEBUG] { name = "bintree"; vars = ["a"; "b"];
    def =
    (Fixpoint (
       [("Leaf", (App ("list", [(Var "a")])));
         ("Node", (Product [(Var "b"); (Var "fixpoint"); (Var "fixpoint")]))],
       "fixpoint"))
    }
  type ('a, 'b) bintree =
    | Leaf of 'a list 
    | Node of 'b * ('a, 'b) bintree * ('a, 'b) bintree [@@deriving zipper]
  type ('a, 'b) bintree_ancestors =
    | NoAncestor 
    | Node0 of 'b * hole * ('a, 'b) bintree * ('a, 'b) bintree_ancestors 
    | Node1 of 'b * ('a, 'b) bintree * hole * ('a, 'b) bintree_ancestors 
  type ('a, 'b) bintree_head =
    | Head of ('a, 'b) bintree 
  type nonrec ('a, 'b) bintree_zipper =
    (('a, 'b) bintree_head * ('a, 'b) bintree_ancestors)
  type ('a, 'b) bintree_poly_zda =
    | Leaf0 of 'a list_d0 * hole 
  type ('a, 'b) bintree_poly_zdb =
    | Node0 of hole * ('a, 'b) bintree * ('a, 'b) bintree 
  type nonrec ('a, 'b) bintree_d0 =
    (('a, 'b) bintree_poly_zda * ('a, 'b) bintree_ancestors)
  type nonrec ('a, 'b) bintree_d1 =
    (('a, 'b) bintree_poly_zdb * ('a, 'b) bintree_ancestors)
  let zip_bintree (x : ('a, 'b) bintree) = ((x, Nil) : ('a, 'b) bintree_zipper)
