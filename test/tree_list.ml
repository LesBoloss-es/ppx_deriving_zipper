type 'a lst = Nil | Cons of 'a * 'a lst
[@@deriving zipper]

type t =
  | Leaf
  | Node of t * t lst
[@@deriving zipper]
