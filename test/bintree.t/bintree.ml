type 'a bintree =
  | Leaf of 'a
  | Node of 'a bintree * 'a bintree
[@@deriving zipper]
