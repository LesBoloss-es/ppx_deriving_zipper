type ('a, 'b) bintree =
  | Leaf of 'a list
  | Node of 'b * ('a, 'b) bintree * ('a, 'b) bintree
[@@deriving zipper]
