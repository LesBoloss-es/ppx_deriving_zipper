type 'a lst = Nil of unit | Cons of 'a * 'a lst
[@@deriving zipper]

type t =
  | Leaf
  | Node of t * t lst
[@@deriving zipper]
