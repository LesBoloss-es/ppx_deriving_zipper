  $ ../standalone.exe tree_list.ml
  [DEBUG] type 'a lst =
    | Nil 
    | Cons of 'a * 'a lst [@@deriving zipper]
  [DEBUG] { name = "lst"; vars = ["a"];
    def =
    (Fixpoint (
       [("Nil", (Product []));
         ("Cons", (Product [(Var "a"); (Var "fixpoint")]))],
       "fixpoint"))
    }
  [DEBUG] type t =
    | Leaf 
    | Node of t * t lst [@@deriving zipper]
  [DEBUG] { name = "t"; vars = [];
    def =
    (Fixpoint (
       [("Leaf", (Product []));
         ("Node",
          (Product [(Var "fixpoint"); (App ("lst", [(Var "fixpoint")]))]))
         ],
       "fixpoint"))
    }
  type 'a lst =
    | Nil 
    | Cons of 'a * 'a lst [@@deriving zipper]
  type 'a lst_ancestors =
    | NoAncestor 
    | Cons0 of 'a * hole * 'a lst_ancestors 
  type 'a lst_head =
    | Head of 'a lst 
  type nonrec 'a lst_zipper = ('a lst_head * 'a lst_ancestors)
  type 'a lst_poly_zda =
    | Cons0 of hole * 'a lst 
  type nonrec 'a lst_d0 = ('a lst_poly_zda * 'a lst_ancestors)
  let zip_lst (x : 'a lst) = ((x, Nil) : 'a lst_zipper)
  type t =
    | Leaf 
    | Node of t * t lst [@@deriving zipper]
  type t_ancestors =
    | NoAncestor 
    | Node0 of hole * t lst * t_ancestors 
    | Node1 of t * (t lst_d0 * hole) * t_ancestors 
  type t_head =
    | Head of t 
    | Node_1 of t * t lst_zipper 
  type nonrec t_zipper = (t_head * t_ancestors)
  let zip_t (x : t) = ((x, Nil) : t_zipper)
