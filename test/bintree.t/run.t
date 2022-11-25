  $ ../standalone.exe ./bintree.ml
  [DEBUG] type 'a bintree =
    | Leaf of 'a 
    | Node of 'a bintree * 'a bintree [@@deriving zipper]
  [DEBUG] { name = "bintree"; vars = ["a"];
    def =
    (Fixpoint (
       [("Leaf", (Var "a"));
         ("Node", (Product [(Var "fixpoint"); (Var "fixpoint")]))],
       "fixpoint"))
    }
  type 'a bintree =
    | Leaf of 'a 
    | Node of 'a bintree * 'a bintree [@@deriving zipper]
  type 'a bintree_ancestors =
    | NoAncestor 
    | Node0 of hole * 'a bintree * 'a bintree_ancestors 
    | Node1 of 'a bintree * hole * 'a bintree_ancestors 
  type 'a bintree_head =
    | Head of 'a bintree 
  type nonrec 'a bintree_zipper = ('a bintree_head * 'a bintree_ancestors)
  type 'a bintree_poly_zda =
    | Leaf0 of hole 
  type nonrec 'a bintree_d0 = ('a bintree_poly_zda * 'a bintree_ancestors)
  let zip_bintree (x : 'a bintree) = ((x, Nil) : 'a bintree_zipper)
