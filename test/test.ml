type tree =
  | Nil
  | Unode of float * tree
  | BNode of int * tree * tree
[@@deriving zipper]
