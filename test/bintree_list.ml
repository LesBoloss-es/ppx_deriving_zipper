type 'a bintree =
  | Leaf of 'a list
  | Node of 'a bintree * 'a bintree
[@@deriving zipper]
